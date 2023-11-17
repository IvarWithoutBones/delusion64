{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , naersk
    , rust-overlay
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      lib = nixpkgs.lib;
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import rust-overlay) ];
      };

      hostPlatform = pkgs.stdenvNoCC.hostPlatform;
      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" "clippy" "rustfmt" ];
      };

      naerskLib = pkgs.callPackage naersk {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

      # Must be kept in sync with what inkwell expects.
      llvmPackages = pkgs.llvmPackages_16;

      # Required for wgpu, it dynamically links to a graphics API.
      libraryPath = lib.optionalString hostPlatform.isLinux
        (lib.makeLibraryPath [
          pkgs.vulkan-loader
        ]);

      delusion64 = naerskLib.buildPackage {
        pname = "delusion64";
        version =
          let
            year = lib.substring 0 4 self.lastModifiedDate;
            month = lib.substring 4 2 self.lastModifiedDate;
            day = lib.substring 6 2 self.lastModifiedDate;
          in
          "0.pre+date=${year}-${month}-${day}";

        src = lib.cleanSource ./.;

        nativeBuildInputs = with pkgs; [
          llvmPackages.bintools # For LLD, only used at build time
        ] ++ lib.optionals hostPlatform.isLinux [
          makeWrapper
          pkg-config
        ];

        buildInputs = with pkgs; [
          llvmPackages.llvm.dev # For the JIT
          libffi
          libxml2
        ] ++ lib.optionals hostPlatform.isLinux [
          # Dependencies of eframe, the GUI library we use
          wayland
          xorg.libXcursor
          xorg.libXrandr
          xorg.libXi
          xorg.libX11
          libxkbcommon
        ] ++ lib.optionals hostPlatform.isDarwin [
          # Also dependencies of eframe
          darwin.apple_sdk.frameworks.AppKit
        ];

        postInstall = lib.optionalString hostPlatform.isLinux ''
          wrapProgram $out/bin/delusion64 --prefix LD_LIBRARY_PATH : ${libraryPath}
        '';
      };
    in
    {
      packages = {
        default = delusion64;
        inherit delusion64;
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ delusion64 ];

        LD_LIBRARY_PATH = lib.optionalString hostPlatform.isLinux libraryPath;

        packages = with pkgs; [
          rustToolchain
          rustToolchain.availableComponents.rust-analyzer

          # Optional debugging tools
          gdb
          just
          cargo-expand
          cargo-flamegraph
          llvmPackages.clang
        ];
      };
    });
}
