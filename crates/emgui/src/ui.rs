use crate::{
    context,
    widget::{error::ErrorDialogue, frame::Frame, input, menu_bar::MenuBar, settings::Settings},
};
use eframe::egui;
use std::time::Duration;

pub const MENU_BAR_HEIGHT: f32 = 15.0;

pub struct Ui<T: input::Event> {
    menu_bar: MenuBar,
    screen: Frame,
    input: input::Handler<T>,
    error: ErrorDialogue,
}

impl<T: input::Event> Ui<T> {
    pub fn new(context: context::UserInterface<T>, input_devices: Vec<T>) -> Self {
        let (input, input_settings) = input::Handler::new(input_devices, context.input);
        let mut settings = Settings::new();
        settings.add(input_settings).unwrap();

        Self {
            menu_bar: MenuBar::new(settings),
            screen: Frame::new(context.framebuffer),
            error: ErrorDialogue::new(context.error),
            input,
        }
    }

    fn update(&mut self, ctx: &egui::Context) {
        #[cfg(feature = "theme")]
        crate::theme::set_theme(ctx, crate::theme::MOCHA);

        self.error.update();
        if self.screen.update(ctx) {
            // Only update input if the screen has changed, so every frame.
            let settings = self.menu_bar.settings.get().unwrap();
            self.input.update(ctx, settings);
        }

        ctx.request_repaint_after(Duration::from_millis(8));
    }
}

impl<T: input::Event> eframe::App for Ui<T> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("menu_bar")
            .exact_height(MENU_BAR_HEIGHT)
            .show(ctx, |ui| {
                self.menu_bar.widget(ui);
            });

        // This must be the last panel we add
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(self.screen.widget());
        });

        self.error.widget(ctx); // TODO: rename to indicate this uses a popup
        self.update(ctx);
    }
}
