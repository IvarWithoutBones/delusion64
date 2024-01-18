use ring_channel::{ring_channel, RingReceiver, RingSender, SendError, TryRecvError};
use std::num::NonZeroUsize;

pub use crate::widget::file::Stop;
pub use crate::widget::{error, input};

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChannelError {
    #[error("all senders have disconnected")]
    SendersDisconnected,
    #[error("all receivers have disconnected")]
    ReceiversDisconnected,
}

#[repr(transparent)]
pub(crate) struct Sender<T>(RingSender<T>);

impl<T> Sender<T> {
    pub fn send(&self, item: T) -> Result<(), ChannelError> {
        self.0
            .send(item)
            .map(|_maybe_old_item| ())
            .map_err(|err| match err {
                SendError::Disconnected(_last_item) => ChannelError::ReceiversDisconnected,
            })
    }
}

#[repr(transparent)]
pub(crate) struct Receiver<T>(RingReceiver<T>);

impl<T> Receiver<T> {
    pub fn receive(&self) -> Result<Option<T>, ChannelError> {
        match self.0.try_recv() {
            Ok(item) => Ok(Some(item)),
            Err(err) => match err {
                TryRecvError::Empty => Ok(None),
                TryRecvError::Disconnected => Err(ChannelError::SendersDisconnected),
            },
        }
    }
}

pub trait SendItem<T> {
    /// Send an item to any receiver without blocking.
    ///
    /// # Errors
    /// An error is returned if all receivers have disconnected.
    fn send(&self, item: T) -> Result<(), ChannelError>;
}

pub trait ReceiveItem<T> {
    /// Receive an item from any sender without blocking.
    /// If an item is available it gets return with `Ok(Some(item))`, if not `Ok(None)` is used to indicate that no item is available.
    ///
    /// # Errors
    /// An error is returned if all senders have disconnected.
    fn receive(&self) -> Result<Option<T>, ChannelError>;
}

macro_rules! item {
    ($item:path, $from:tt . $from_field:ident => $to:tt . $to_field:ident) => {
        impl<T: input::Event> SendItem<$item> for $from<T> {
            fn send(&self, item: $item) -> Result<(), ChannelError> {
                self.$from_field.send(item)
            }
        }

        impl<T: input::Event> ReceiveItem<$item> for $to<T> {
            fn receive(&self) -> Result<Option<$item>, ChannelError> {
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
item!(Stop, UserInterface.stop => Emulator.stop);

pub struct Emulator<T: input::Event> {
    framebuffer: Sender<Framebuffer>,
    input: Receiver<input::DeviceState<T>>,
    error: Sender<error::Error>,
    stop: Receiver<Stop>,
}

pub struct UserInterface<T: input::Event> {
    pub(crate) framebuffer: Receiver<Framebuffer>,
    pub(crate) input: Sender<input::DeviceState<T>>,
    pub(crate) error: Receiver<error::Error>,
    pub(crate) stop: Sender<Stop>,
}

#[must_use]
pub fn channel<T: input::Event>() -> (Emulator<T>, UserInterface<T>) {
    const CAP: NonZeroUsize = unsafe { NonZeroUsize::new_unchecked(1) };
    let (framebuffer_tx, framebuffer_rx) = ring_channel(CAP);
    let (input_tx, input_rx) = ring_channel(CAP);
    let (error_tx, error_rx) = ring_channel(CAP);
    let (stop_tx, stop_rx) = ring_channel(CAP);

    (
        Emulator {
            framebuffer: Sender(framebuffer_tx),
            input: Receiver(input_rx),
            error: Sender(error_tx),
            stop: Receiver(stop_rx),
        },
        UserInterface {
            framebuffer: Receiver(framebuffer_rx),
            input: Sender(input_tx),
            error: Receiver(error_rx),
            stop: Sender(stop_tx),
        },
    )
}
