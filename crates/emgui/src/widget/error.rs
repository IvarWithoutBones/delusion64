use crate::context::Receiver;
use eframe::egui;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Error {
    message: String,
}

impl Error {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

pub(crate) struct ErrorDialogue {
    error: Option<Error>,
    receiver: Receiver<Error>,
}

impl ErrorDialogue {
    pub fn new(receiver: Receiver<Error>) -> Self {
        Self {
            error: None,
            receiver,
        }
    }

    pub fn update(&mut self) {
        if let Some(error) = self.receiver.receive().expect("channel closed") {
            self.error = Some(error);
        }
    }

    pub fn widget(&mut self, ctx: &egui::Context) {
        if let Some(error) = &mut self.error {
            egui::Window::new("Fatal Error")
                .auto_sized()
                .collapsible(false)
                .show(ctx, |ui| ui.label(error.message.clone()));
        }
    }
}
