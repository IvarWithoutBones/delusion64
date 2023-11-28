use super::settings::Settings;
use eframe::egui;

pub trait Item {
    fn name(&self) -> &'static str;

    fn menu_items(&mut self, ui: &mut egui::Ui);

    fn widget(&mut self, _ui: &mut egui::Ui) {}
}

pub struct File;

impl Item for File {
    fn name(&self) -> &'static str {
        "File"
    }

    fn menu_items(&mut self, ui: &mut egui::Ui) {
        if ui.button("Open").clicked() {
            // TODO: Actually open a file. We might want to own an EmulatorHandle object with functions like `run(rom: &[u8])`, etc which spawn an emu thread.
            // One complication is later killing that thread. The standard library unfortunately provides no cross-platform way to kill threads from a JoinHandle.
            // On Unix platforms we could call `pthread_cancel`, what about Windows though?
            ui.close_menu();
        }

        if ui.button("Exit").clicked() {
            std::process::exit(0);
        }
    }
}

pub struct MenuBar {
    items: Vec<Box<dyn Item>>,
    // TODO: move this
    pub settings: Settings,
}

impl MenuBar {
    pub fn new(settings: Settings) -> Self {
        Self {
            items: vec![Box::new(File)],
            settings,
        }
    }

    pub fn widget(&mut self, ui: &mut egui::Ui) {
        egui::menu::bar(ui, |ui| {
            for item in &mut self.items {
                ui.menu_button(item.name(), |ui| {
                    item.menu_items(ui);
                });

                ui.menu_button(self.settings.name(), |ui| {
                    self.settings.menu_items(ui);
                });
            }
        });

        for item in &mut self.items {
            item.widget(ui);
        }

        self.settings.widget(ui);
    }
}
