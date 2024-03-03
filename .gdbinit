# This is done lazily so that we can still debug the emulator itself as well, instead of the guest
define d64-init-guest
    define llvm-ir-opt
        init-if-undefined $llvmIrOptCmd = "pipe monitor llvm-ir | opt -S --O3"
        eval "echo $ %s\n", $llvmIrOptCmd
        eval "%s", $llvmIrOptCmd
    end

    set endian big
    set remotetimeout 9999

    echo Connecting to localhost:9001\n
    target remote localhost:9001
end

# See the justfile for helpers to instantiate either of these
if $_isvoid($d64_target_rsp) == 0
    echo Using RSP mode\n
    d64-init-guest
end

if $_isvoid($d64_target_cpu) == 0
    echo Using CPU mode\n
    set mips abi n64
    d64-init-guest
end
