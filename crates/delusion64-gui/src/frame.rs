use crate::context::{Framebuffer, Receiver};
use eframe::egui;

pub struct Frame {
    frame: Option<egui::TextureHandle>,
    buffer_receiver: Receiver<Framebuffer>,
}

impl Frame {
    pub fn new(buffer_receiver: Receiver<Framebuffer>) -> Self {
        Self {
            frame: None,
            buffer_receiver,
        }
    }

    pub fn update(&mut self, ctx: &egui::Context) -> bool {
        if let Some(fb) = &self.buffer_receiver.receive() {
            self.frame = Some(ctx.load_texture(
                "frame",
                egui::ColorImage::from_rgba_unmultiplied([fb.width, fb.height], &fb.pixels),
                egui::TextureOptions::NEAREST,
            ));
            true
        } else {
            false
        }
    }

    pub fn widget(&mut self) -> impl egui::Widget + '_ {
        move |ui: &mut egui::Ui| {
            ui.horizontal(|ui| {
                if let Some(frame) = &self.frame {
                    ui.image(frame);
                }
            })
            .response
        }
    }
}
