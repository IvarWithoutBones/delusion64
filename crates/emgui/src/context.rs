// TODO: switch to a bounded mpsc/spsc channel, using a ringbuffer to only store the item last send.

pub use crate::widget::{error, input};
use std::sync::mpsc;

#[repr(transparent)]
pub(crate) struct Sender<T>(mpsc::Sender<T>);

impl<T> Sender<T> {
    pub fn send(&self, item: T) -> Result<(), mpsc::SendError<T>> {
        self.0.send(item)
    }
}

#[repr(transparent)]
pub(crate) struct Receiver<T>(mpsc::Receiver<T>);

impl<T> Receiver<T> {
    pub fn receive(&self) -> Option<T> {
        self.0.try_iter().last()
    }
}

pub trait SendItem<T> {
    fn send(&self, item: T) -> Option<()>;
}

pub trait ReceiveItem<T> {
    fn receive(&self) -> Option<T>;
}

macro_rules! item {
    ($item:path, $from:tt . $from_field:ident => $to:tt . $to_field:ident) => {
        impl<T: input::Event> SendItem<$item> for $from<T> {
            fn send(&self, item: $item) -> Option<()> {
                self.$from_field.send(item).ok()
            }
        }

        impl<T: input::Event> ReceiveItem<$item> for $to<T> {
            fn receive(&self) -> Option<$item> {
                self.$to_field.receive()
            }
        }
    };
}

pub struct Framebuffer {
    pub width: usize,
    pub height: usize,
    pub pixels: Box<[u8]>,
}

item!(Framebuffer, Emulator.framebuffer => UserInterface.framebuffer);
item!(error::Error, Emulator.error => UserInterface.error);
item!(input::DeviceState<T>, UserInterface.input => Emulator.input);

pub struct Emulator<T: input::Event> {
    framebuffer: Sender<Framebuffer>,
    input: Receiver<input::DeviceState<T>>,
    error: Sender<error::Error>,
}

pub struct UserInterface<T: input::Event> {
    pub(crate) framebuffer: Receiver<Framebuffer>,
    pub(crate) input: Sender<input::DeviceState<T>>,
    pub(crate) error: Receiver<error::Error>,
}

#[must_use]
pub fn channel<T: input::Event>() -> (Emulator<T>, UserInterface<T>) {
    let (framebuffer_tx, framebuffer_rx) = mpsc::channel();
    let (input_tx, input_rx) = mpsc::channel();
    let (error_tx, error_rx) = mpsc::channel();

    (
        Emulator {
            framebuffer: Sender(framebuffer_tx),
            input: Receiver(input_rx),
            error: Sender(error_tx),
        },
        UserInterface {
            framebuffer: Receiver(framebuffer_rx),
            input: Sender(input_tx),
            error: Receiver(error_rx),
        },
    )
}
