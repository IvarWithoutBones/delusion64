{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    rust-overlay.url = "github:oxalica/rust-overlay";
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
      llvm = pkgs.llvmPackages_15.llvm;
      mipsBinutils = pkgs.pkgsCross.mips64-linux-gnuabi64.buildPackages.binutilsNoLibc;

      compile-llc = pkgs.writeShellScriptBin "compile-ll" ''
        set -euo pipefail

        INPUT="''${1?"Usage: compile-ll <input.ll> <output.o> [stripped-output.bin]"}"
        OUTPUT="''${2?"Usage: compile-ll <input.ll> <output.o> [stripped-output.bin]"}"
        STRIPPED_OUTPUT="''${3:-}"

        set -x

        # Compile the LLVM IR
        ${llvm}/bin/llc -relocation-model=pic -filetype obj -O2 -mtriple=mips64 -mcpu=mips3 -o "$OUTPUT" "$INPUT"

        if test -n "$STRIPPED_OUTPUT"; then
          # Copy the raw contents of the .text section to the output file, stripping the ELF headers
          ${mipsBinutils}/bin/mips64-unknown-linux-gnuabi64-objcopy -O binary --only-section=.text "$OUTPUT" "$STRIPPED_OUTPUT"
        fi

        # Show a disassembly of the generated code
        ${mipsBinutils}/bin/mips64-unknown-linux-gnuabi64-objdump "$OUTPUT" \
          --architecture mips:4300 \
          --disassemble \
          --disassembler-color=on \
          --disassembler-options=no-aliases \
          --visualize-jumps=extended-color
      '';

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
          llvm.dev
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
        inherit delusion64 compile-llc;
      };

      devShells.default = pkgs.mkShell {
        inputsFrom = [ delusion64 ];

        packages = [
          rustToolchain
          rustToolchain.availableComponents.rust-analyzer
          mipsBinutils
          compile-llc
        ];
      };
    });
}
