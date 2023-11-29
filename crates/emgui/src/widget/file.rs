use super::menu_bar;
use crate::{context::Sender, EmulatorHandle};
use eframe::egui;
use std::path::PathBuf;

pub struct Stop;

#[derive(Debug, PartialEq)]
enum State {
    WaitingForRom { path: Option<PathBuf> },
    Running,
}

pub(crate) struct File<T: EmulatorHandle> {
    handle: T,
    stop: Sender<Stop>,
    state: State,
}

impl<T: EmulatorHandle> File<T> {
    pub fn new(handle: T, initial_rom_path: Option<PathBuf>, stop: Sender<Stop>) -> Self {
        Self {
            handle,
            stop,
            state: State::WaitingForRom {
                path: initial_rom_path,
            },
        }
    }

    pub fn is_running(&self) -> bool {
        self.state == State::Running
    }

    pub fn update(&mut self) {
        if let State::WaitingForRom { path } = &mut self.state {
            if let Some(path) = path.take() {
                let rom = std::fs::read(path).unwrap().into_boxed_slice();
                self.handle.start(rom);
                self.state = State::Running;
            }
        }
    }

    fn stop(&mut self) {
        self.stop.send(Stop).unwrap();
        self.handle.stop();
        self.state = State::WaitingForRom { path: None };
    }
}

impl<T: EmulatorHandle> menu_bar::Item for File<T> {
    fn name(&self) -> &'static str {
        "File"
    }

    fn menu_items(&mut self, ui: &mut egui::Ui) {
        if ui.button("Open").clicked() {
            ui.close_menu();
            if self.is_running() {
                self.stop();
            }

            if let Some(path) = rfd::FileDialog::new().pick_file() {
                self.state = State::WaitingForRom { path: Some(path) };
            }
        }

        if self.state == State::Running && ui.button("Close").clicked() {
            ui.close_menu();
            self.stop();
        }

        if ui.button("Exit").clicked() {
            std::process::exit(0);
        }
    }
}
