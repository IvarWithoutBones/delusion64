alias ir := llvm-ir
alias bin := compile-ll
alias dis := disassemble
alias swap := endian-swap

# Show a disassembly of a MIPS ELF file.
disassemble elf *args:
    mips64-unknown-linux-gnuabi64-objdump "{{elf}}" \
      --architecture mips:4300 \
      --disassemble-all \
      --disassembler-color=on \
      --disassembler-options=no-aliases \
      --visualize-jumps=extended-color \
      {{args}}

# Builds a human-readable LLVM IR file from a C/C++ source.
llvm-ir input-c output-ll *args:
    clang -S -emit-llvm -mcpu=mips3 -target mips64 "{{input-c}}" -o "{{output-ll}}" {{args}}

# Compiles an LLVM IR file to a flat binary (only the .text section).
compile-ll input-ll output-bin:
    #! /usr/bin/env bash
    set -euo pipefail

    # Create a temporary file for the generated ELF
    TMPFILE="$(mktemp -t compiled-llc-elf.XXXXXX)"
    trap 'rm -rf "$TMPFILE"' EXIT INT TERM

    set -x

    # Compile the LLVM IR
    llc \
        -filetype obj \
        -relocation-model=pic \
        -mtriple=mips64 -mcpu=mips3 \
        -O2 \
        -o "$TMPFILE" "{{input-ll}}"

    # Copy the raw contents of the .text section to the output file, stripping the ELF headers
    mips64-unknown-linux-gnuabi64-objcopy \
        -O binary \
        --only-section=.text \
        "$TMPFILE" "{{output-bin}}"

    just disassemble "$TMPFILE"

# Endian swaps a binary file, useful for older homebrew ROMs.
endian-swap input-bin output-bin num-bytes="4":
    objcopy --target binary --reverse-bytes={{num-bytes}} "{{input-bin}}" "{{output-bin}}"
