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

      rustToolchain = pkgs.rust-bin.stable.latest.default.override {
        extensions = [ "rust-src" "clippy" "rustfmt" ];
      };

      naerskLib = pkgs.callPackage naersk {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

      # NOTE: LLVM version must be kept in sync with what inkwell expects!
      llvmPackages = pkgs.llvmPackages_15;

      # TODO: Could we use LLVM binutils instead?
      mipsBinutils = pkgs.pkgsCross.mips64-linux-gnuabi64.buildPackages.binutilsNoLibc;

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

        nativeBuildInputs = [
          llvmPackages.llvm.dev
        ];

        buildInputs = with pkgs; [
          libffi
          libxml2
        ];
      };
    in
    {
      packages = {
        default = delusion64;
        inherit delusion64;
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ delusion64 ];

        packages = [
          rustToolchain
          rustToolchain.availableComponents.rust-analyzer

          # Optional debugging tools
          pkgs.just
          pkgs.gdb
          llvmPackages.clang
          mipsBinutils
        ];
      };
    });
}
