define llvm-ir-opt
    init-if-undefined $llvmIrOptCmd = "pipe monitor llvm-ir | opt -S --O3"
    eval "echo $ %s\n", $llvmIrOptCmd
    eval "%s", $llvmIrOptCmd
end

set endian big
set mips abi n64

set remotetimeout 9999
echo connecting to localhost:9001\n
target remote localhost:9001
