use crate::{context::Sender, widget::settings};
use eframe::egui;
use std::fmt;

pub type DeviceID = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Button,
    Joystick,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EventInfo<T> {
    pub input_type: Type,
    pub event: T,
    pub name: &'static str,
}

impl<T: Event> EventInfo<T> {
    #[must_use]
    pub fn new(input_type: Type, event: T, name: &'static str) -> Self {
        Self {
            input_type,
            event,
            name,
        }
    }
}

pub trait Event: Clone {
    #[must_use]
    fn info(&self) -> Vec<EventInfo<Self>>;
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonState {
    Pressed,
    #[default]
    Released,
}

pub struct EventState<T: Event> {
    pub event: T,
    // TODO: support joystick data
    pub state: ButtonState,
}

pub struct DeviceState<T: Event> {
    pub events: Box<[EventState<T>]>,
    pub device_id: DeviceID,
}

impl<T: Event> DeviceState<T> {
    pub fn active(&self) -> impl Iterator<Item = &T> {
        self.events.iter().filter_map(|event| match event.state {
            ButtonState::Pressed => Some(&event.event),
            ButtonState::Released => None,
        })
    }
}

pub(crate) struct Handler<T: Event> {
    info: Box<[Vec<EventInfo<T>>]>,
    input_sender: Sender<DeviceState<T>>,
}

impl<T: Event> Handler<T> {
    #[must_use]
    pub fn new(devices: Vec<T>, input_sender: Sender<DeviceState<T>>) -> (Self, Settings) {
        let info: Box<[Vec<EventInfo<T>>]> =
            devices.into_iter().map(|device| device.info()).collect();
        let settings = Settings::new(&info);
        (Self { info, input_sender }, settings)
    }

    fn devices<'a>(
        &'a self,
        settings: &'a Settings,
    ) -> impl Iterator<Item = (usize, impl Iterator<Item = (&EventInfo<T>, &'a Binding)>)> + '_
    {
        self.info
            .iter()
            .enumerate()
            .zip(settings.devices.iter())
            .map(|((device_id, device), bindings)| {
                (
                    device_id,
                    device
                        .iter()
                        .zip(bindings.iter().map(|(_name, binding)| binding)),
                )
            })
    }

    pub fn update(&mut self, egui_ctx: &egui::Context, settings: &Settings) {
        egui_ctx.input(|input| {
            for (device_id, events_iter) in self.devices(settings) {
                let events = events_iter
                    .filter_map(|(info, binding)| match binding {
                        Binding::Key(key) => Some(EventState {
                            event: info.event.clone(),
                            state: if input.key_down(*key) {
                                ButtonState::Pressed
                            } else {
                                ButtonState::Released
                            },
                        }),
                        Binding::None => None,
                    })
                    .collect();
                self.input_sender
                    .send(DeviceState { events, device_id })
                    .unwrap();
            }
        });
    }
}

fn get_pressed_key(egui_ctx: &egui::Context) -> Option<egui::Key> {
    egui_ctx.input(|input| {
        input.events.iter().find_map(|e| match e {
            egui::Event::Key {
                key,
                pressed: true,
                repeat: false,
                ..
            } => Some(*key),
            _ => None,
        })
    })
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Binding {
    // TODO: controller support
    Key(egui::Key),
    #[default]
    None,
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(key) => write!(f, "{key:?}"),
            Self::None => write!(f, "None"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum RemappingState {
    #[default]
    Idle,
    WaitingForInput(usize, usize),
}

pub(crate) struct Settings {
    devices: Box<[Vec<(&'static str, Binding)>]>,
    state: RemappingState,
}

impl Settings {
    fn new<T: Event>(info: &[Vec<EventInfo<T>>]) -> Self {
        Self {
            state: RemappingState::default(),
            devices: info
                .iter()
                .map(|dev| dev.iter().map(|info| (info.name, Binding::None)).collect())
                .collect(),
        }
    }
}

impl settings::Item for Settings {
    fn name(&self) -> &'static str {
        "Input"
    }

    fn widget(&mut self, ui: &mut egui::Ui) {
        egui::Grid::new("input").show(ui, |ui| {
            for (device, event, (name, mapping)) in
                self.devices
                    .iter_mut()
                    .enumerate()
                    .flat_map(|(device_id, device)| {
                        device
                            .iter_mut()
                            .enumerate()
                            .map(move |(idx, binding)| (device_id, idx, binding))
                    })
            {
                ui.label(*name);

                let button = ui.add_enabled(
                    self.state == RemappingState::Idle,
                    egui::Button::new(mapping.to_string()),
                );

                if button.clicked() {
                    self.state = RemappingState::WaitingForInput(device, event);
                } else if self.state == RemappingState::WaitingForInput(device, event) {
                    if let Some(key) = get_pressed_key(ui.ctx()) {
                        *mapping = Binding::Key(key);
                        self.state = RemappingState::Idle;
                    }
                }

                ui.end_row();
            }
        });
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
