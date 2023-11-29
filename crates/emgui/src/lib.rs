#![warn(clippy::all, clippy::pedantic)]

use std::path::PathBuf;

use crate::{
    ui::Ui,
    widget::{input, menu_bar},
};
use eframe::egui;

pub mod context;
mod ui;
pub(crate) mod widget;

#[cfg(feature = "theme")]
mod theme;

pub trait EmulatorHandle {
    type InputEvent: input::Event;

    fn start(&mut self, rom: Box<[u8]>);

    fn stop(&mut self);
}

pub struct UiBuilder<T: EmulatorHandle> {
    handle: T,
    context: context::UserInterface<T::InputEvent>,
    window_name: String,
    initial_window_size: Option<egui::Vec2>,
    input_devices: Vec<T::InputEvent>,
    rom_path: Option<PathBuf>,
}

impl<T: EmulatorHandle + 'static> UiBuilder<T> {
    #[must_use]
    pub fn new(
        window_name: impl Into<String>,
        handle: T,
        context: context::UserInterface<T::InputEvent>,
    ) -> Self {
        Self {
            context,
            window_name: window_name.into(),
            input_devices: vec![],
            handle,
            initial_window_size: None,
            rom_path: None,
        }
    }

    #[must_use]
    pub fn with_input_devices(self, devices: Vec<T::InputEvent>) -> UiBuilder<T> {
        UiBuilder {
            input_devices: devices,
            ..self
        }
    }

    #[must_use]
    pub fn with_initial_screen_size(self, [width, height]: [f32; 2]) -> Self {
        // Offsets are required since the screen does not fill the whole GUI, while we unfortunately cannot know the dimensions in advance.
        const OFFSET: f32 = 15.0;
        Self {
            initial_window_size: Some(egui::Vec2::new(
                width + OFFSET,
                height + OFFSET + menu_bar::HEIGHT,
            )),
            ..self
        }
    }

    #[must_use]
    pub fn with_rom_path(self, path: impl Into<PathBuf>) -> Self {
        Self {
            rom_path: Some(path.into()),
            ..self
        }
    }

    /// Start running the UI, and block until the user closes the window.
    /// # Panics
    /// This function will panic if the UI fails to initialize. This can happen when the UI is not spawned on the main thread.
    pub fn run(self) {
        let native_options = eframe::NativeOptions {
            viewport: egui::ViewportBuilder {
                inner_size: self.initial_window_size,
                ..Default::default()
            },
            follow_system_theme: false,
            ..Default::default()
        };

        eframe::run_native(
            &self.window_name.clone(),
            native_options,
            Box::new(move |_cc| {
                Box::new(Ui::new(
                    &self.window_name,
                    self.context,
                    self.input_devices,
                    self.handle,
                    self.rom_path,
                ))
            }),
        )
        .unwrap_or_else(|e| {
            panic!("failed to run eframe: {e}");
        });
    }
}
