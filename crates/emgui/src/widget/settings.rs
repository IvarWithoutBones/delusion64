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

struct Viewport {
    id: egui::ViewportId,
    builder: egui::ViewportBuilder,
    opened: bool,
    selected_category: usize,
}

impl Viewport {
    const WINDOW_SIZE: egui::Vec2 = egui::vec2(400.0, 400.0);
    const ITEMS_PANEL_WIDTH: f32 = 125.0;

    pub fn new(window_name: &str) -> Self {
        let window_name = format!("{window_name} - Settings");
        Self {
            opened: false,
            selected_category: 0,
            id: egui::ViewportId::from_hash_of("settings_viewport"),
            builder: egui::ViewportBuilder::default()
                .with_title(window_name)
                .with_inner_size(Self::WINDOW_SIZE),
        }
    }

    fn items_panel(&mut self, ui: &mut egui::Ui, items: &[Box<dyn Item>]) {
        ui.add_space(6.0);
        ui.with_layout(
            egui::Layout::top_down_justified(egui::Align::Center),
            |ui| {
                ui.heading("Settings");
                ui.separator();
                for (i, item) in items.iter().enumerate() {
                    ui.selectable_value(&mut self.selected_category, i, item.name());
                }
            },
        );
    }

    pub fn widget(&mut self, ctx: &egui::Context, items: &mut [Box<dyn Item>]) {
        if self.opened {
            ctx.show_viewport_immediate(self.id, self.builder.clone(), |ctx, _class| {
                egui::SidePanel::left("items")
                    .default_width(Self::ITEMS_PANEL_WIDTH)
                    .show(ctx, |ui| self.items_panel(ui, items));

                egui::CentralPanel::default().show(ctx, |ui| {
                    if let Some(item) = items.get_mut(self.selected_category) {
                        item.widget(ui);
                    }
                });

                if ctx.input(|i| i.viewport().close_requested()) {
                    self.opened = false;
                    self.selected_category = 0;
                };
            });
        }
    }
}

pub struct Settings {
    viewport: Viewport,
    items: Vec<Box<dyn Item>>,
}

impl Settings {
    pub fn new(window_name: &str) -> Self {
        Self {
            viewport: Viewport::new(window_name),
            items: vec![],
        }
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
        self.viewport.opened = true;
        ui.close_menu();
    }

    fn widget(&mut self, ui: &mut egui::Ui) {
        self.viewport.widget(ui.ctx(), &mut self.items);
    }
}
