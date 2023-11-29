use crate::{
    context,
    widget::{
        error::ErrorDialogue, file::File, frame::Frame, input, menu_bar::MenuBar,
        settings::Settings,
    },
    EmulatorHandle,
};
use eframe::egui;
use std::{path::PathBuf, time::Duration};

pub struct Ui<T: EmulatorHandle> {
    menu_bar: MenuBar,
    frame: Frame,
    input: input::Handler<T::InputEvent>,
    error: ErrorDialogue,
    settings: Settings,
    file: File<T>,
}

impl<T: EmulatorHandle> Ui<T> {
    pub fn new(
        window_name: &str,
        context: context::UserInterface<T::InputEvent>,
        input_devices: Vec<T::InputEvent>,
        handle: T,
        initial_rom_path: Option<PathBuf>,
    ) -> Self {
        let mut settings = Settings::new(window_name);
        let (input, input_settings) = input::Handler::new(input_devices, context.input);
        settings.add(input_settings).unwrap();

        Self {
            menu_bar: MenuBar::new(vec![]),
            frame: Frame::new(context.framebuffer),
            error: ErrorDialogue::new(context.error),
            file: File::new(handle, initial_rom_path, context.stop),
            settings,
            input,
        }
    }

    fn update(&mut self, ctx: &egui::Context) {
        #[cfg(feature = "theme")]
        crate::theme::set_theme(ctx, crate::theme::MOCHA);

        self.file.update();
        self.error.update();

        if self.frame.update(ctx) {
            // Only update input if the screen has changed, so every frame.
            self.input.update(ctx, self.settings.get().unwrap());
        }

        if !self.file.is_running() {
            // Ensure we never render the last frame rendered when we were running.
            self.frame.clear();
        }

        ctx.request_repaint_after(Duration::from_millis(1));
    }
}

impl<T: EmulatorHandle> eframe::App for Ui<T> {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Creates a TopBottomPanel, so must be added before the CentralPanel.
        self.menu_bar
            .widget(ctx, &mut [&mut self.file, &mut self.settings]);

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.add(self.frame.widget());
        });

        self.error.widget(ctx); // TODO: rename to indicate this uses a popup
        self.update(ctx);
    }
}
