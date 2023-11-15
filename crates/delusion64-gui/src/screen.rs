use crate::context::{self, ReceiveItem};
use eframe::egui;

struct Frame(egui::TextureHandle);

impl Frame {
    fn new(fb: &context::Framebuffer, egui_ctx: &egui::Context) -> Self {
        Self(egui_ctx.load_texture(
            "framebuffer",
            egui::ColorImage::from_rgba_unmultiplied([fb.width, fb.height], &fb.pixels),
            egui::TextureOptions::NEAREST,
        ))
    }
}

#[derive(Default)]
pub struct Screen {
    frame: Option<Frame>,
}

impl Screen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn draw(
        &mut self,
        emu_ctx: &context::UserInterface,
        egui_ctx: &egui::Context,
    ) -> impl egui::Widget + '_ {
        if let Some(fb) = &emu_ctx.receive() {
            self.frame = Some(Frame::new(fb, egui_ctx));
        }

        move |ui: &mut egui::Ui| {
            ui.horizontal(|ui| {
                if let Some(frame) = &self.frame {
                    ui.image(&frame.0);
                }
            })
            .response
        }
    }
}
