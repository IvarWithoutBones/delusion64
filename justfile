# Builds a human-readable LLVM IR file from a C/C++ source.
llvm-ir input-c output-ll *args:
    clang -S -emit-llvm -mcpu=mips3 -target mips64 "{{input-c}}" -o "{{output-ll}}" {{args}}

# Compiles an LLVM IR file to an ELF binary.
compile-ll input-ll output-bin:
    llc -filetype obj -relocation-model=pic -mtriple=mips64 -mcpu=mips3 -O2 -o "{{output-bin}}" "{{input-ll}}"

gdb target *args="":
    {{if target == "cpu" { "" } else if target == "rsp" { "" } else { error("invalid target") } }}
    @# Will enable target-specific settings in .gdbinit
    @gdb --init-eval-command 'set $d64_target_{{target}} = 1' {{args}}

cargo_flamegraph_flags := "--palette hot" + if os() == "macos" { " --root" } else { "" }

flamegraph *args:
    cargo flamegraph {{cargo_flamegraph_flags}} {{args}}

flamechart *args:
    cargo flamegraph --flamechart {{cargo_flamegraph_flags}} {{args}}
