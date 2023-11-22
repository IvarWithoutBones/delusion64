#![warn(clippy::all, clippy::pedantic)]

use crate::{ui::Ui, widget::input};
use eframe::egui;

pub mod context;
mod ui;
pub(crate) mod widget;

pub struct UiBuilder<T: input::Event> {
    context: context::UserInterface<T>,
    window_name: String,
    initial_window_size: Option<egui::Vec2>,
    input_devices: Vec<T>,
}

impl<T: input::Event + 'static> UiBuilder<T> {
    #[must_use]
    pub fn new(window_name: impl Into<String>, context: context::UserInterface<T>) -> Self {
        Self {
            context,
            window_name: window_name.into(),
            initial_window_size: None,
            input_devices: vec![],
        }
    }

    #[must_use]
    pub fn with_input_devices(self, devices: Vec<T>) -> UiBuilder<T> {
        UiBuilder {
            context: self.context,
            window_name: self.window_name,
            initial_window_size: self.initial_window_size,
            input_devices: devices,
        }
    }

    #[must_use]
    pub fn with_initial_window_size(self, [width, height]: [f32; 2]) -> Self {
        // Offsets are required since the screen does not fill the whole GUI, while we unfortunately cannot know the dimensions in advance.
        const OFFSET: f32 = 10.0;
        Self {
            initial_window_size: Some(egui::Vec2::new(
                width + OFFSET,
                height + OFFSET + ui::MENU_BAR_HEIGHT,
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
            Box::new(|_cc| Box::new(Ui::new(self.context, self.input_devices))),
        )
        .unwrap_or_else(|e| {
            panic!("failed to run eframe: {e}");
        });
    }
}
