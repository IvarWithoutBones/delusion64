use eframe::egui;

pub const HEIGHT: f32 = 15.0;

pub trait Item {
    fn name(&self) -> &'static str;

    fn menu_items(&mut self, ui: &mut egui::Ui);

    fn widget(&mut self, _ui: &mut egui::Ui) {}
}

pub struct MenuBar {
    items: Vec<Box<dyn Item>>,
}

impl MenuBar {
    pub fn new(items: impl Into<Vec<Box<dyn Item>>>) -> Self {
        Self {
            items: items.into(),
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
