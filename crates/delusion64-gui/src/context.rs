// TODO: crossbeams array queue would be useful for bounded channels:
// https://docs.rs/crossbeam-queue/latest/crossbeam_queue/struct.ArrayQueue.html#method.force_push

use std::sync::mpsc;

pub trait Item {}

pub trait SendItem<T: Item> {
    fn send(&self, item: T) -> Option<()>;
}

pub trait ReceiveItem<T: Item> {
    fn receive(&self) -> Option<T>;
}

macro_rules! item {
    ($item:path, $from:tt . $from_field:ident => $to:tt . $to_field:ident) => {
        impl Item for $item {}

        impl SendItem<$item> for $from {
            fn send(&self, item: $item) -> Option<()> {
                self.$from_field.send(item).ok()
            }
        }

        impl ReceiveItem<$item> for $to {
            fn receive(&self) -> Option<$item> {
                self.$to_field.try_iter().last()
            }
        }
    };
}

pub struct Framebuffer {
    pub width: usize,
    pub height: usize,
    pub pixels: Box<[u8]>,
}

// TODO: implement
pub struct Buttons;

item!(Framebuffer, Emulator.framebuffer => UserInterface.framebuffer);
item!(Buttons, UserInterface.buttons => Emulator.buttons);

pub struct Emulator {
    framebuffer: mpsc::Sender<Framebuffer>,
    buttons: mpsc::Receiver<Buttons>,
}

pub struct UserInterface {
    framebuffer: mpsc::Receiver<Framebuffer>,
    buttons: mpsc::Sender<Buttons>,
}

#[must_use]
pub fn channel() -> (Emulator, UserInterface) {
    let (framebuffer_tx, framebuffer_rx) = mpsc::channel();
    let (buttons_tx, buttons_rx) = mpsc::channel();
    (
        Emulator {
            framebuffer: framebuffer_tx,
            buttons: buttons_rx,
        },
        UserInterface {
            framebuffer: framebuffer_rx,
            buttons: buttons_tx,
        },
    )
}
