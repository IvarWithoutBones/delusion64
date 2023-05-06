## N64 Resources
* General information, probably the best source
    - https://n64brew.dev/wiki/Main_Page

* General information wiki
    - http://en64.shoutwiki.com/wiki/Main_Page

* Boot code analysis
    - https://www.retroreversing.com/n64bootcode

* Architecture overview (author also has other interesting articles)
    - https://www.copetti.org/writings/consoles/nintendo-64

* Common RSP microcodes
    - https://www.retroreversing.com/n64rsp#pre-written-microcodes

* MIPS 3 ISA documentation
    - https://n64brew.dev/wiki/MIPS_III_instructions

* N64 CPU + RCP ISA specifications in a DSL
    - https://github.com/ARM9/bass/blob/c5866a54cc5734ef4da338dbf712211f0ff6a56f/bass/data/architectures/n64.cpu.arch

* Ultra64 documentation
    - https://ultra64.ca/resources/documentation

* VR43xx CPU manual
    - http://datasheets.chipdb.org/NEC/Vr-Series/Vr43xx/U10504EJ7V0UMJ1.pdf

* Notes and documentation of various components
    - https://github.com/Dillonb/n64-resources
    - https://github.com/Dillonb/n64/tree/master/docs/source

* Test ROMs
    - https://github.com/PeterLemon/N64

* Reality Signal Processor (RSP)
    - https://github.com/rasky/r64emu/blob/5d95a7cacbcae483d1a3f4865297813843c1e26e/doc/rsp.md

* Reality Display Processor (RDP) implementation using `vulkan`, could be integrated
    - https://github.com/Themaister/parallel-rdp

* Emulator in Rust (WIP)
    - https://github.com/rasky/r64emu

* Cycle-accurate emulator
    - https://github.com/n64dev/cen64

* List of additional resources
    - https://n64.dev

## JIT Resources
A basic interpreter would be the path of least resistance, although it would be fun to do a JIT.

* RISC-V emulator using `cranelift` as a JIT
    - https://github.com/Amanieu/a-tale-of-binary-translation

* LLVM JIT framework API
    - https://llvm.org/docs/ORCv2.html

* Dynamic binary lifting and recompilation papers
    - https://www.ics.uci.edu/~dabrowsa/altinaydabrowski-eurosys20-binrec.pdf
    - https://monkbai.github.io/files/sp-22.pdf

* Dynamic binary translation talk
    - https://www.youtube.com/watch?v=Avp55U2JFcQ&pp

* LLVM-based dynamic binary instrumentation project
    - https://github.com/aengelke/instrew
    - Associated papers:
        - https://home.in.tum.de/~engelke/pubs/2104-vee.pdf
        - https://home.in.tum.de/~engelke/pubs/2003-vee.pdf
        - https://mediatum.ub.tum.de/doc/1614897/1614897.pdf

* Recompilation engine of the 1964 N64 emulator
    - https://emudev.org/docs/1964-recompiling-engine-documentation.pdf

* Cached interpreter for the GBA
    - https://emudev.org/2021/01/31/cached-interpreter.html

* Series of examples on building a MIPS JIT
    - https://github.com/RichardBrown384/mips-jit-tutorial

* MIPS to LLVM IR re-compiler blog
    - http://themaister.net/blog/2019/01/27/an-unusual-recompiler-experiment-mips-to-llvm-ir-part-1
    - source code (MIT): https://github.com/Themaister/MIPS-LLVM-Jitter

* LLVM JIT for emulation thesis
    - https://llvm.org/pubs/2010-01-Wennborg-Thesis.pdf

* CPU Simulation using JIT Binary Translation paper
    - http://www-mount.ece.umn.edu/~jjyi/MoBS/2007/program/02A-Topham.pdf

* Generic JIT tutorial
    - https://github.com/spencertipping/jit-tutorial

* Simple JIT blog
    - https://blog.reverberate.org/2012/12/hello-jit-world-joy-of-simple-jits.html

* `NES` JIT article
    - https://bheisler.github.io/post/experiments-in-nes-jit-compilation

* `NES` Static recompilation using LLVM
    - https://andrewkelley.me/post/jamulator.html

* 6502 JIT article
    - https://jahej.com/alt/2011_06_12_jit-cpu-emulation-a-6502-to-x86-dynamic-recompiler-part-1.html

* Dynamic re-compiler tutorial
    - https://multigesture.net/wp-content/uploads/mirror/zenogais/Dynamic%20Recompiler.html

* PCSX2 (PS2 emulator) dynamic recompilation introduction
    - https://wiki.pcsx2.net/PCSX2_Documentation/Introduction_to_Dynamic_Recompilation

* JIT for emulation of LEON2/LEON3 processors paper (from the European Space Agency, lol)
    - https://indico.esa.int/event/109/contributions/208/attachments/306/344/2034476_meurant.PDF

* `eBPF` JIT for 32-bit MIPS
    - https://lwn.net/Articles/863745

* `eBPF` JIT port blog
    - https://blog.trailofbits.com/2022/10/12/solana-jit-compiler-ebpf-arm64
