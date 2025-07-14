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

      cargoTomlContents = builtins.readFile ./Cargo.toml;

      version = (builtins.fromTOML cargoTomlContents).workspace.package.version;
      rustVersion = (builtins.fromTOML cargoTomlContents).workspace.package."rust-version";

      rustToolchain = pkgs.rust-bin.stable.${rustVersion}.default;

      rustPlatform = pkgs.makeRustPlatform {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

      aiken = rustPlatform.buildRustPackage {
        inherit version;

        name = "aiken";

        buildInputs = with pkgs; [openssl] ++ osxDependencies;
        nativeBuildInputs = with pkgs; [pkg-config openssl.dev];

        src = pkgs.lib.cleanSourceWith {src = self;};
        doCheck = false; # don’t run cargo test
        CARGO_BUILD_TESTS = "false"; # don’t even compile test binaries

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
            (rustToolchain.override {
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
