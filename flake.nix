# This setup relies on 'Cargo.nix' to be up-to-date.
#
# It can be re-generated using:
#
#     nix run github:cargo2nix/cargo2nix
#
{
  inputs = {
    cargo2nix = {
      url = "github:cargo2nix/cargo2nix/release-0.11.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
    devshell.url = "github:numtide/devshell";
  };

  outputs = {
    self,
    cargo2nix,
    rust-overlay,
    nixpkgs,
    flake-utils,
    devshell,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [cargo2nix.overlays.default devshell.overlays.default];
        };

        deno = nixpkgs.legacyPackages.${system}.deno;

        rustPkgs = pkgs.rustBuilder.makePackageSet {
          rustVersion = "1.69.0";
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
        gitRev = if (builtins.hasAttr "rev" self)
            then self.rev
            else "dirty";
      in {
        inherit packages;
        devShell = rustPkgs.workspaceShell {
          packages = [deno];
          shellHook =
          ''
            export GIT_REVISION=${gitRev}
          '';
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
