use emgui::context::input::{self, DeviceState};
use n64_si::controller::StandardController;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControllerEvent {
    A,
    B,
    Z,
    Start,
    DpadUp,
    DpadDown,
    DpadLeft,
    DpadRight,
    CUp,
    CDown,
    CLeft,
    CRight,
    JoystickX,
    JoystickY,
}

impl input::Event for ControllerEvent {
    fn info(&self) -> Vec<input::EventInfo<Self>> {
        use input::{EventInfo, Type};
        vec![
            EventInfo::new(Type::Button, Self::A, "A"),
            EventInfo::new(Type::Button, Self::B, "B"),
            EventInfo::new(Type::Button, Self::Z, "Z"),
            EventInfo::new(Type::Button, Self::Start, "Start"),
            EventInfo::new(Type::Button, Self::DpadUp, "Dpad Up"),
            EventInfo::new(Type::Button, Self::DpadDown, "Dpad Down"),
            EventInfo::new(Type::Button, Self::DpadLeft, "Dpad Left"),
            EventInfo::new(Type::Button, Self::DpadRight, "Dpad Right"),
            EventInfo::new(Type::Button, Self::CUp, "C Up"),
            EventInfo::new(Type::Button, Self::CDown, "C Down"),
            EventInfo::new(Type::Button, Self::CLeft, "C Left"),
            EventInfo::new(Type::Button, Self::CRight, "C Right"),
            EventInfo::new(Type::Joystick, Self::JoystickX, "Joystick X"),
            EventInfo::new(Type::Joystick, Self::JoystickY, "Joystick Y"),
        ]
    }
}

/// Newtype wrapper for the standard controller, so that we can implement traits on it.
#[repr(transparent)]
pub struct Controller(StandardController);

impl From<DeviceState<ControllerEvent>> for Controller {
    fn from(state: DeviceState<ControllerEvent>) -> Self {
        let mut controller = StandardController::default();
        for event in state.active() {
            match event {
                ControllerEvent::A => controller.set_a(true),
                ControllerEvent::B => controller.set_b(true),
                ControllerEvent::Z => controller.set_z(true),
                ControllerEvent::Start => controller.set_start(true),
                ControllerEvent::DpadUp => controller.set_dpad_up(true),
                ControllerEvent::DpadDown => controller.set_dpad_down(true),
                ControllerEvent::DpadLeft => controller.set_dpad_left(true),
                ControllerEvent::DpadRight => controller.set_dpad_right(true),
                ControllerEvent::CUp => controller.set_c_up(true),
                ControllerEvent::CDown => controller.set_c_down(true),
                ControllerEvent::CLeft => controller.set_c_left(true),
                ControllerEvent::CRight => controller.set_c_right(true),
                _ => {}
            }
        }
        Self(controller)
    }
}

impl From<Controller> for StandardController {
    fn from(controller: Controller) -> Self {
        controller.0
    }
}
