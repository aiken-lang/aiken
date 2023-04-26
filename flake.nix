# This setup relies on 'Cargo.nix' to be up-to-date.
#
# It can be re-generated using:
#
#     nix run github:cargo2nix/cargo2nix
#
{
  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix/release-0.11.0";
    flake-utils.follows = "cargo2nix/flake-utils";
    nixpkgs.follows = "cargo2nix/nixpkgs";
    devshell.url = "github:numtide/devshell";
    unstable.url = "github:nixos/nixpkgs";
  };

  outputs = {
    self,
    cargo2nix,
    nixpkgs,
    flake-utils,
    devshell,
    unstable,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [cargo2nix.overlays.default devshell.overlays.default];
        };

        deno = unstable.legacyPackages.${system}.deno;

        rustPkgs = pkgs.rustBuilder.makePackageSet {
          rustVersion = "1.66.1";
          packageFun = import ./Cargo.nix;
        };

        commonCategory = y: builtins.map (x: x // {category = y;});

        packages = {
          aiken = (rustPkgs.workspace.aiken {}).bin;
          default = packages.aiken;
        };

        aikenCmds = commonCategory "Aiken Development" [
          {
            name = "aiken";
            help = "Aiken toolchain";
            package = packages.aiken;
          }
        ];
      in rec {
        inherit packages;
        devShell = rustPkgs.workspaceShell {
          packages = [deno];
        };
        devShells = {
          aiken = pkgs.devshell.mkShell {
            name = "aiken";
            motd = ''              Aiken
                                        $(type -p menu &>/dev/null && menu)'';
            commands = aikenCmds;
          };
        };
      }
    );
}
