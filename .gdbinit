# This is done lazily so that we can still debug the emulator itself as well, instead of the guest
define d64-init-guest
    define llvm-ir-opt
        pipe monitor llvm-ir | opt -S --O3 | bat --plain --color always --language llvm
    end

    set endian big
    set remotetimeout 9999

    echo Connecting to the emulator ($arg0 mode) at localhost:9001\n
    target remote localhost:9001
end

# See the justfile for helpers to instantiate either of these
if $_isvoid($d64_target_rsp) == 0
    d64-init-guest RSP
end

if $_isvoid($d64_target_cpu) == 0
    set mips abi n64
    d64-init-guest CPU
end
