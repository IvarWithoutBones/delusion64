#![warn(clippy::all, clippy::pedantic)]

use self::screen::Screen;
use eframe::egui;

pub mod context;
mod screen;

pub struct UiBuilder {
    context: context::UserInterface,
    window_name: String,
    initial_window_size: Option<egui::Vec2>,
}

impl UiBuilder {
    #[must_use]
    pub fn new(window_name: impl Into<String>, context: context::UserInterface) -> Self {
        Self {
            context,
            window_name: window_name.into(),
            initial_window_size: None,
        }
    }

    #[must_use]
    pub fn with_initial_window_size(self, [width, height]: [usize; 2]) -> Self {
        // Offsets are required since the screen does not fill the whole GUI, while we unfortunately cannot know the dimensions in advance.
        const WIDTH_OFFSET: f32 = 16.0;
        const HEIGHT_OFFSET: f32 = 13.0;

        #[allow(clippy::cast_precision_loss)]
        let (width, height) = (width as f32, height as f32);
        Self {
            initial_window_size: Some(egui::Vec2::new(
                width + WIDTH_OFFSET,
                height + HEIGHT_OFFSET,
            )),
            ..self
        }
    }

    /// Start running the UI, and block until the user closes the window.
    /// # Panics
    /// This function will panic if the UI fails to initialize. This can happen when the UI is not spawned on the main thread.
    pub fn run(self) {
        let native_options = eframe::NativeOptions {
            initial_window_size: self.initial_window_size,
            follow_system_theme: false,
            ..Default::default()
        };

        eframe::run_native(
            &self.window_name,
            native_options,
            Box::new(|_cc| Box::new(Ui::new(self.context))),
        )
        .unwrap_or_else(|e| {
            panic!("failed to run eframe: {e}");
        });
    }
}

struct Ui {
    context: context::UserInterface,
    screen: Screen,
}

impl Ui {
    fn new(context: context::UserInterface) -> Self {
        Self {
            context,
            screen: Screen::new(),
        }
    }
}

impl eframe::App for Ui {
    fn update(&mut self, egui_ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(egui_ctx, |ui| {
            ui.add(self.screen.draw(&self.context, egui_ctx));
        });

        // Immediately request another draw, otherwise we have to wait for the user to interact with the window.
        egui_ctx.request_repaint();
    }
}
