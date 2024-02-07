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
        extensions = [ "rust-src" "clippy" "rustfmt" "rust-analyzer" ];
      };

      naerskLib = pkgs.callPackage naersk {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

      # Must be kept in sync with what inkwell expects.
      llvmPackages = pkgs.llvmPackages_16;

      mkShell = pkgs.mkShell.override {
        stdenv = llvmPackages.stdenv;
      };

      libraryPath = lib.optionalString hostPlatform.isLinux
        (lib.makeLibraryPath (with pkgs; [ vulkan-loader libxkbcommon ]));

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
          # Purely used for the lld linker, which is significantly faster than GNU ld, especially when performing LTO.
          # Note that we cannot use the `llvmPackages.lld` package, as it is not wrapped to set the rpath of generated executables.
          llvmPackages.bintools
        ] ++ lib.optionals hostPlatform.isLinux [
          makeWrapper
          wrapGAppsHook
          pkg-config
        ];

        buildInputs = with pkgs; [
          # LLVM and its dependencies, used by the JIT
          llvmPackages.llvm.dev
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
          # Dependencies of rfd, provides a file picker
          pango
          gdk-pixbuf
          atk
          gtk3
          libz
        ] ++ lib.optionals hostPlatform.isDarwin [
          # Also dependencies of eframe
          darwin.apple_sdk.frameworks.AppKit
        ];

        dontWrapGApps = true;

        postInstall = lib.optionalString hostPlatform.isLinux ''
          # Manually include the arguments from wrapGAppsHook to avoid double wrapping
          wrapProgram $out/bin/delusion64 ''${gappsWrapperArgs[@]} --prefix LD_LIBRARY_PATH : ${libraryPath}
        '';
      };
    in
    {
      packages = {
        default = delusion64;
        inherit delusion64;
      };

      devShells.default = mkShell {
        inputsFrom = [ delusion64 ];

        LD_LIBRARY_PATH = lib.optionalString hostPlatform.isLinux libraryPath;

        shellHook = ''
          # Ensure we can find the relevant GSettings schemas, otherwise we crash when opening the file picker
          export XDG_DATA_DIRS="$XDG_DATA_DIRS:$GSETTINGS_SCHEMAS_PATH"
        '';

        packages = with pkgs; [
          rustToolchain

          # Optional debugging tools
          gdb
          just
          cargo-outdated
          cargo-expand
          cargo-flamegraph
          llvmPackages.clang
        ];
      };
    });
}
