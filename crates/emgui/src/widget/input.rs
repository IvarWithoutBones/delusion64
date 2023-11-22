use crate::context::Sender;
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

pub trait Event: Sized + Clone {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Mapping {
    key: egui::Key,
    state: ButtonState,
}

impl fmt::Display for Mapping {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.state {
            ButtonState::Pressed => write!(f, "{:?} (pressed)", self.key),
            ButtonState::Released => write!(f, "{:?} (released)", self.key),
        }
    }
}

struct Device<T: Event> {
    events: Vec<(EventInfo<T>, Option<Mapping>)>,
}

impl<T: Event> Device<T> {
    fn update_events(&mut self, ctx: &egui::Context) {
        ctx.input(|input| {
            for (_info, mapping) in &mut self.events {
                if let Some(mapping) = mapping {
                    mapping.state = if input.key_down(mapping.key) {
                        ButtonState::Pressed
                    } else {
                        ButtonState::Released
                    }
                }
            }
        });
    }

    fn event_states(&self) -> Box<[EventState<T>]> {
        self.events
            .iter()
            .filter_map(|(info, mapping)| {
                mapping.map(|mapping| EventState {
                    event: info.event.clone(),
                    state: mapping.state,
                })
            })
            .collect()
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum MappingState {
    #[default]
    Idle,
    WaitingForInput(usize),
}

pub(crate) struct Handler<T: Event> {
    devices: Box<[Device<T>]>,
    state: MappingState,
    input_sender: Sender<DeviceState<T>>,
}

impl<T: Event> Handler<T> {
    #[must_use]
    pub fn new(devices: Vec<T>, input_sender: Sender<DeviceState<T>>) -> Self {
        Self {
            devices: devices
                .into_iter()
                .map(|device| Device {
                    events: device.info().into_iter().map(|info| (info, None)).collect(),
                })
                .collect(),
            state: MappingState::default(),
            input_sender,
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

    pub fn update(&mut self, egui_ctx: &egui::Context) {
        for (device_id, device) in self.devices.iter_mut().enumerate() {
            device.update_events(egui_ctx);
            self.input_sender
                .send(DeviceState {
                    events: device.event_states(),
                    device_id,
                })
                .unwrap_or_else(|e| {
                    eprintln!("failed to send input: {e}");
                    std::process::exit(1);
                });
        }
    }

    pub fn widget(&mut self) -> impl egui::Widget + '_ {
        move |ui: &mut egui::Ui| {
            ui.vertical(|ui| {
                let mut next_state = self.state;
                for (idx, (info, mapping)) in self
                    .devices
                    .iter_mut()
                    .flat_map(|d| &mut d.events)
                    .enumerate()
                {
                    let mapping_name = if let Some(mapping) = mapping {
                        format!("{}: {mapping}", info.name)
                    } else {
                        format!("{}: <unmapped>", info.name)
                    };

                    let button = ui.add_enabled(
                        next_state == MappingState::Idle,
                        egui::Button::new(mapping_name),
                    );

                    if button.clicked() {
                        // This can only happen if we are not in the process of remapping already, since we disable the button in that case.
                        next_state = MappingState::WaitingForInput(idx);
                    } else if next_state == MappingState::WaitingForInput(idx) {
                        if let Some(key) = Self::get_pressed_key(ui.ctx()) {
                            *mapping = Some(Mapping {
                                key,
                                state: ButtonState::Pressed,
                            });
                            next_state = MappingState::Idle;
                        }
                    }
                }
                self.state = next_state;
            })
            .response
        }
    }
}
