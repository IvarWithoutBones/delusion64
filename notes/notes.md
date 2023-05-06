## Notes
Cartridges typically ship a microkernel for scheduling, multitasking, etc.
    - Nintendo SDK allegedly ships one.

All memory accesses go over a unified BUS, a unified-memory architecture (UMA).

* MIPS III CPU
    - Has both 64/32 bit modes, register width matching.
    - 32-bit address bus.
    - Contains address space isolation
    - 5-stage pipeline (?).
    - Used both for the central CPU, and the "scalar unit" of the RSP where some functionality is restricted.
      Should implement the `cpu` crate in a way generic enough to support both.

* Audio Interface (AI)
    - Transfers waveform data to the digital-to-analogue converter (DAC).
    - Can produce two channels containing 16-bit waveforms.

* Reality Co-Processing Unit (RCP)
    - Contains the RSP and RDP.
    - For vertex processing, the CPU typically follows these steps:
        - Create a "Display List" containing instructions for the RSP, point the RSP to it.
        - Send microcode to the RSP which the Scalar Unit will start executing.
    - For pixel processing, the CPU needs to:
        - Tell the RDP to rasterize vectors.
        - Copy textures into RDP texture memory.
        - Upload the frame computed by the RDP to the Video Interface (VI), preferably using DMA.

* Reality Signal Processor (RSP)
    - Communicated with using memory-mapped IO.
    - Geometry engine uses microcode for its graphics pipeline, fed by the CPU at runtime.
    - Usage for graphics:
        - Geometry transformations, clipping, lighting, etc.
        - Sends the resulting rasterization commands to the RDP once it's finished, using its own bus (`XBUS`).
    - Usage for audio (using microcode):
        - Decompressing `ADPCM` samples and applying effects, sequencing and mixing MIDI data.
        - Sends the resulting waveform data to the Audio Interface (AI).

* Reality Display Processor (RDP)
    - Works with triangles or rectangles, called primitives.
    - Can receive commands from the RSP over `XBUS`, or from memory-mapped IO.
    - Contains a Z-Buffer, stores the depth of the nearest pixel relative to the camera.
    - Contains 4 KB of texture memory, for the Texture Unit.
    - No microcode.
    - Composes frames with the following components:
        - Rasterizing, converts vertices into pixels.
            - Compares the new pixels with the Z-Buffer, and discards if its behind something already rendered.
        - Color combiner, mixes and interpolates colors.
        - Blender, combines pixels with the current frame buffer for translucency, etc.
        - Memory interface, for the frame buffer and texture memory.
            - Utilizes 9th-bit special case on the bus. (what for?)
        - Texture unit, can do a few jobs:
            - Bilinear filtering, maps a 2D texture over a 3D shape.
            - Mip-Mapping, scales down textures depending on the specified level of detail.
            - Perspective correction, maps textures onto triangles (?).
    - Once a frame is finished processing, it's copied into the designated frame buffer segment in memory.

* Parallel Bus (PBUS/PI)
    - 16-bit
    - Communicated with by the RCP.
    - Optional interface to additional memory contained on the cartridge (usually saves).

* PIF-NUS (PIF)
    - Communicated with by the RCP.
    - Handles some security, and add-ons connected to the controller.

* Initial Program Load (IPL)
    - The PIF-NUS stalls the main CPU, until the CIC validates the cartridge is authentic.
    - Start executing stage one (IPL1) from the PIF-NUS ROM.
        - Partially initialize hardware like CPU registers, the RCP, etc.
        - Copy IPL2 from PIF-NUS ROM into RSP memory for faster access.
    - Start executing stage two (IPL2)
        - Initialize RDRAM and the CPU cache.
        - Copy the first MB of the games ROM into RDRAM, which contains IPL3.
    - Start executing stage three (IPL3)
        - Typically starts the operating system, initializes virtual memory and trap handlers.
            - Since this is provided with the cartridge it varies from vendor to vendor.
        - If there is no Disk Drive attached, call the game's entry point
    - If there is a Disk Drive attached, start executing stage four (IPL4)
        - Unsure, presumably just initializes the Disk Drive?
