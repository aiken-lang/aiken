{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    rust-overlay,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [rust-overlay.overlays.default];
      };

      osxDependencies = with pkgs;
        lib.optionals stdenv.isDarwin
        [
          darwin.apple_sdk.frameworks.Security
          darwin.apple_sdk.frameworks.CoreServices
          darwin.apple_sdk.frameworks.SystemConfiguration
        ];

      cargoTomlContents = builtins.readFile ./crates/aiken/Cargo.toml;
      version = (builtins.fromTOML cargoTomlContents).package.version;

      aiken = pkgs.rustPlatform.buildRustPackage {
        inherit version;

        name = "aiken";

        buildInputs = with pkgs; [openssl] ++ osxDependencies;
        nativeBuildInputs = with pkgs; [pkg-config openssl.dev];

        src = pkgs.lib.cleanSourceWith {src = self;};

        cargoLock.lockFile = ./Cargo.lock;

        GIT_COMMIT_HASH_SHORT = self.shortRev or "unknown";
        postPatch = ''
          substituteInPlace crates/aiken-lang/src/version.rs \
            --replace  "built_info::GIT_COMMIT_HASH_SHORT" \
            "Some(\"$GIT_COMMIT_HASH_SHORT\")"
        '';

        postInstall = ''
          mkdir -p $out/share/zsh/site-functions
          $out/bin/aiken completion zsh > $out/share/zsh/site-functions/_aiken

          mkdir -p $out/share/bash-completion/completions
          $out/bin/aiken completion bash > $out/share/bash-completion/completions/aiken

          mkdir -p $out/share/fish/vendor_completions.d
          $out/bin/aiken completion fish > $out/share/fish/vendor_completions.d/aiken.fish
        '';

        meta = with pkgs.lib; {
          description = "Cardano smart contract language and toolchain";
          homepage = "https://github.com/aiken-lang/aiken";
          license = licenses.asl20;
          mainProgram = "aiken";
        };
      };

      packages = {
        aiken = aiken;
        default = packages.aiken;
      };

      overlays.default = final: prev: {aiken = packages.aiken;};

      gitRev =
        if (builtins.hasAttr "rev" self)
        then self.rev
        else "dirty";
    in {
      inherit packages overlays;

      devShell = pkgs.mkShell {
        buildInputs = with pkgs;
          [
            pkg-config
            openssl
            cargo-insta
            (pkgs.rust-bin.stable.latest.default.override {
              extensions = ["rust-src" "clippy" "rustfmt" "rust-analyzer"];
            })
          ]
          ++ osxDependencies;

        shellHook = ''
          export GIT_REVISION=${gitRev}
        '';
      };
    });
}
