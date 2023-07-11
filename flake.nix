{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, rust-overlay, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        osxDependencies = with pkgs;
          lib.optionals stdenv.isDarwin
          [ darwin.apple_sdk.frameworks.Security ];

        deno = nixpkgs.legacyPackages.${system}.deno;

        cargoTomlContents = builtins.readFile ./crates/aiken/Cargo.toml;
        version = (builtins.fromTOML cargoTomlContents).package.version;

        aiken = pkgs.rustPlatform.buildRustPackage {
          inherit version;

          name = "aiken";

          buildInputs = with pkgs; [ openssl ] ++ osxDependencies;
          nativeBuildInputs = with pkgs; [ pkg-config openssl.dev ];
          cargoBuildFlags = [ "--package aiken" ];

          src = pkgs.lib.cleanSourceWith { src = self; };

          cargoLock.lockFile = ./Cargo.lock;

          meta = with pkgs.lib; {
            description = "Cardano smart contract language and toolchain";
            homepage = "https://github.com/aiken-lang/aiken";
            license = licenses.asl20;
            mainProgram = "aiken";
          };
        };

        commonCategory = y: builtins.map (x: x // { category = y; });

        packages = {
          aiken = aiken;
          default = packages.aiken;
        };

        overlays.default = final: prev: { aiken = packages.aiken; };

        aikenCmds = commonCategory "Aiken Development" [{
          name = "aiken";
          help = "Aiken toolchain";
          package = packages.aiken;
        }];

        gitRev = if (builtins.hasAttr "rev" self) then self.rev else "dirty";
      in {
        inherit packages overlays;

        devShells.aiken = pkgs.mkShell {
          name = "aiken";
          motd = ''
            Aiken
            $(type -p menu &>/dev/null && menu)'';
          commands = aikenCmds;

          packages = [
            deno

            (pkgs.rust-bin.stable.latest.default.override {
              extensions = [ "rust-src" "clippy" "rustfmt" ];
            })
          ] ++ osxDependencies;

          shellHook = ''
            export GIT_REVISION=${gitRev}
          '';
        };
      });
}
