use super::menu_bar;
use eframe::egui;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Error {
    DuplicateItem,
    UnknownItem,
}

pub trait Item {
    fn name(&self) -> &'static str;

    fn widget(&mut self, ui: &mut egui::Ui);

    fn as_any(&self) -> &dyn std::any::Any;
}

#[derive(Default)]
pub struct Settings {
    opened: bool,
    items: Vec<Box<dyn Item>>,
}

impl Settings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add<T: Item + 'static>(&mut self, item: T) -> Result<(), Error> {
        if self
            .items
            .iter()
            .any(|existing| existing.as_any().type_id() == item.as_any().type_id())
        {
            Err(Error::DuplicateItem)
        } else {
            self.items.push(Box::new(item));
            Ok(())
        }
    }

    pub fn get<T: Item + 'static>(&mut self) -> Result<&T, Error> {
        self.items
            .iter()
            .find_map(|item| item.as_any().downcast_ref())
            .ok_or(Error::UnknownItem)
    }
}

impl menu_bar::Item for Settings {
    fn name(&self) -> &'static str {
        "Settings"
    }

    fn menu_items(&mut self, ui: &mut egui::Ui) {
        self.opened = true;
        ui.close_menu();
    }

    fn widget(&mut self, ui: &mut egui::Ui) {
        // TODO: switch this to a native window once support for that makes it into a release. See this PR:
        // https://github.com/emilk/egui/pull/3172
        egui::Window::new("Settings")
            .open(&mut self.opened)
            .collapsible(false)
            .resizable(false)
            .auto_sized()
            .show(ui.ctx(), |ui| {
                for item in &mut self.items {
                    ui.label(egui::RichText::new(item.name()).strong());
                    item.widget(ui);
                }
            });
    }
}
