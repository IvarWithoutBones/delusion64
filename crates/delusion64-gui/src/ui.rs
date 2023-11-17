use crate::{context, frame::Frame, input};
use eframe::egui;
use std::time::Duration;

pub struct Ui<T: input::Event> {
    screen: Frame,
    input: input::Handler<T>,
}

impl<T: input::Event> Ui<T> {
    pub fn new(context: context::UserInterface<T>, input_devices: Vec<T>) -> Self {
        Self {
            screen: Frame::new(context.framebuffer),
            input: input::Handler::new(input_devices, context.input),
        }
    }

    fn update(&mut self, ctx: &egui::Context) {
        if self.screen.update(ctx) {
            // Only update input if the screen has changed, so every frame.
            self.input.update(ctx);
        }
        ctx.request_repaint_after(Duration::from_millis(8));
    }
}

impl<T: input::Event> eframe::App for Ui<T> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(self.input.widget());
            ui.add(self.screen.widget());
        });

        self.update(ctx);
    }
}
