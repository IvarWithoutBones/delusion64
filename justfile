alias ir := llvm-ir
alias bin := compile-ll
alias dis := disassemble

# Ensures the terminal is in a sane state upon exit (it might not be when GDB crashes).
gdb:
    gdb --quiet || stty -onlcr

# Show a disassembly of a MIPS ELF file.
disassemble elf:
    mips64-unknown-linux-gnuabi64-objdump "{{elf}}" \
      --architecture mips:4300 \
      --disassemble \
      --disassembler-color=on \
      --disassembler-options=no-aliases \
      --visualize-jumps=extended-color

# Builds a human-readable LLVM IR file from a C/C++ source.
llvm-ir input-c output-ll:
    clang \
        -S -emit-llvm \
        -mcpu=mips3 -target mips64 \
        "{{input-c}}" -o "{{output-ll}}"

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
