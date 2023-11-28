use crate::{
    context,
    widget::{error::ErrorDialogue, frame::Frame, input, menu_bar::MenuBar, settings::Settings},
};
use eframe::egui;
use std::time::Duration;

pub struct Ui<T: input::Event> {
    menu_bar: MenuBar,
    screen: Frame,
    input: input::Handler<T>,
    error: ErrorDialogue,
    settings: Settings,
}

impl<T: input::Event> Ui<T> {
    pub fn new(context: context::UserInterface<T>, input_devices: Vec<T>) -> Self {
        let mut settings = Settings::new();
        let (input, input_settings) = input::Handler::new(input_devices, context.input);
        settings.add(input_settings).unwrap();

        Self {
            menu_bar: MenuBar::new(),
            screen: Frame::new(context.framebuffer),
            error: ErrorDialogue::new(context.error),
            settings,
            input,
        }
    }

    fn update(&mut self, ctx: &egui::Context) {
        #[cfg(feature = "theme")]
        crate::theme::set_theme(ctx, crate::theme::MOCHA);

        self.error.update();
        if self.screen.update(ctx) {
            // Only update input if the screen has changed, so every frame.
            self.input.update(ctx, self.settings.get().unwrap());
        }

        ctx.request_repaint_after(Duration::from_millis(1));
    }
}

impl<T: input::Event> eframe::App for Ui<T> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Creates a TopBottomPanel, so must be added before the CentralPanel.
        self.menu_bar.widget(ctx, &mut [&mut self.settings]);

        // This must be the last panel we add
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(self.screen.widget());
        });

        self.error.widget(ctx); // TODO: rename to indicate this uses a popup
        self.update(ctx);
    }
}
