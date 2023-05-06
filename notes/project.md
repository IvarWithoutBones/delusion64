## Structure
Split components into their own crates as much as possible.

A `bin` for the binary, containing the main loop ticking the CPU, etc.
The actual logic is implemented in the following libraries:

* `common` provides traits for reading/writing memory
    - Could contain stuff like save states and debugging facilities like logging too
* `bus` holds every component that is either memory mapped or has memory access
    - Components should be registered here with a trait from `common`
* `cpu` is the MIPS interpreter, registered for reads/writes with the `bus`
* `cart` has a ROM parser (`.z64`)
* `gui` contains everything user-facing
    - Except maybe the renderer? Would potentially be useful to abstract out

## GUI
Want to use `gtk4-rs`. (https://github.com/gtk-rs/gtk4-rs)

Advantages:
* Native looking and feeling interface
* Mature and high quality wrapper
* Would need to depend on it for a file dialog on Linux anyway
* Has a `OpenGL` draw area example (https://github.com/gtk-rs/gtk4-rs/tree/82e14841ae549d39af61c46f625cecefccca02e7/examples/glium_gl_area)
    - `vulkan` is also supported, although it is experimental (?) and no examples are provided.

Disadvantages:
* No native controller support, would need SDL2-input integration
* No WebAssembly support, although that probably would not be performant anyway.
* Non-immediate mode GUIs are painful to write.
