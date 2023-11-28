use eframe::egui;

pub const HEIGHT: f32 = 15.0;

pub trait Item {
    fn name(&self) -> &'static str;

    fn menu_items(&mut self, ui: &mut egui::Ui);

    fn widget(&mut self, _ui: &mut egui::Ui) {}
}

struct File;

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
}

impl MenuBar {
    pub fn new() -> Self {
        Self {
            items: vec![Box::new(File)],
        }
    }

    fn bar(&mut self, ui: &mut egui::Ui, builtin_items: &mut [&mut dyn Item]) {
        egui::menu::bar(ui, |ui| {
            for item in self
                .items
                .iter_mut()
                .map(|item| item.as_mut() as &mut dyn Item)
                .chain(builtin_items.iter_mut().map(|item| *item as &mut dyn Item))
            {
                ui.menu_button(item.name(), |ui| {
                    item.menu_items(ui);
                });
            }
        });

        for item in self
            .items
            .iter_mut()
            .map(|item| item.as_mut() as &mut dyn Item)
            .chain(builtin_items.iter_mut().map(|item| *item as &mut dyn Item))
        {
            item.widget(ui);
        }
    }

    pub fn widget(&mut self, ctx: &egui::Context, builtin_items: &mut [&mut dyn Item]) {
        egui::TopBottomPanel::top("menu_bar")
            .exact_height(HEIGHT)
            .show(ctx, |ui| {
                self.bar(ui, builtin_items);
            });
    }
}
