use crate::runtime::{memory::tlb::AccessMode, Environment, Memory};
use gdbstub::outputln;
use std::{collections::HashMap, num::ParseIntError};

pub type MonitorCommandMap<'ctx, Mem> = HashMap<&'static str, Command<'ctx, Mem>>;

pub enum Command<'ctx, Mem: Memory> {
    Internal(MonitorCommand<Environment<'ctx, Mem>>),
    External(MonitorCommand<Mem>),
}

impl<'ctx, Mem: Memory> Command<'ctx, Mem> {
    /// Merges internal and external monitor commands into a single map.
    pub fn monitor_command_map(external: Vec<MonitorCommand<Mem>>) -> MonitorCommandMap<'ctx, Mem> {
        Environment::monitor_commands()
            .into_iter()
            .map(|cmd| (cmd.name, Command::Internal(cmd)))
            .chain(
                external
                    .into_iter()
                    .map(|cmd| (cmd.name, Command::External(cmd))),
            )
            .collect()
    }

    pub fn handle(
        &mut self,
        env: &mut Environment<'ctx, Mem>,
        output: &mut dyn std::fmt::Write,
        args: &mut dyn Iterator<Item = &str>,
    ) -> Result<(), MonitorCommandHandlerError> {
        match self {
            Command::Internal(cmd) => (cmd.handler)(env, output, args),
            Command::External(cmd) => (cmd.handler)(&env.memory, output, args),
        }
    }
}

/// An error that can occur when handling a monitor command.
pub enum MonitorCommandHandlerError {
    InvalidArgument(String),
    Other(String),
}

impl std::fmt::Debug for MonitorCommandHandlerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonitorCommandHandlerError::InvalidArgument(arg) => {
                write!(f, "invalid argument {arg:?}")
            }
            MonitorCommandHandlerError::Other(msg) => write!(f, "{msg}"),
        }
    }
}

impl<'a> From<&'a str> for MonitorCommandHandlerError {
    fn from(value: &'a str) -> Self {
        Self::Other(value.to_owned())
    }
}

impl From<String> for MonitorCommandHandlerError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl From<std::fmt::Error> for MonitorCommandHandlerError {
    fn from(_: std::fmt::Error) -> Self {
        Self::Other("failed to write to output".to_owned())
    }
}

impl From<()> for MonitorCommandHandlerError {
    fn from(_: ()) -> Self {
        Self::Other("failed to handle command".to_owned())
    }
}

/// A callback to handle a monitor command.
pub type MonitorCommandHandler<This> = dyn FnMut(
    &This,
    &mut dyn std::fmt::Write,
    &mut dyn Iterator<Item = &str>,
) -> Result<(), MonitorCommandHandlerError>;

pub struct MonitorCommand<This> {
    pub name: &'static str,
    pub handler: Box<MonitorCommandHandler<This>>,
}

impl<Mem: Memory> Environment<'_, Mem> {
    /// The internal monitor commands, related to the CPU state.
    pub(crate) fn monitor_commands() -> Vec<MonitorCommand<Self>> {
        vec![
            MonitorCommand {
                name: "regs",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "status",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers.status())?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "tlb",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.tlb)?;
                    Ok(())
                }),
            },
            MonitorCommand {
                name: "paddr",
                handler: Box::new(|env, out, args| {
                    let vaddr = str_to_u64(args.next().ok_or("expected virtual address")?)
                        .map_err(|err| format!("invalid virtual address: {err}"))?;
                    let paddr = env
                        .tlb
                        .translate(vaddr, AccessMode::Read, &env.registers)
                        .map_err(|err| {
                            format!("virtual address {vaddr:#x} is not mapped: {err:#?}")
                        })?;
                    outputln!(out, "{paddr:#x}");
                    Ok(())
                }),
            },
        ]
    }
}

impl<Mem: Memory> gdbstub::target::ext::monitor_cmd::MonitorCmd for Environment<'_, Mem> {
    fn handle_monitor_cmd(
        &mut self,
        cmd: &[u8],
        mut out: gdbstub::target::ext::monitor_cmd::ConsoleOutput<'_>,
    ) -> Result<(), Self::Error> {
        // This is a closure so that we can early-return using `?`, without propagating the error.
        let _ = || -> Result<(), ()> {
            let cmd = std::str::from_utf8(cmd).map_err(|_| {
                outputln!(out, "monitor command is not valid UTF-8");
            })?;
            let mut iter = cmd.split_whitespace().peekable();

            // Juggle around the debugger to appease the borrow checker
            let mut debugger = self.debugger.take().unwrap();
            if let Some(name) = iter.next() {
                if let Some(cmd) = debugger.monitor_commands.get_mut(name) {
                    cmd.handle(self, &mut out, &mut iter).unwrap_or_else(|err| {
                        outputln!(out, "{err:#?}");
                    });
                } else {
                    outputln!(out, "unrecognized command: '{cmd}'");
                }
            } else {
                outputln!(out, "no command specified");
            }
            self.debugger = Some(debugger);

            if iter.peek().is_some() {
                outputln!(out, "warning: ignoring extra arguments");
            }
            Ok(())
        }();
        Ok(())
    }
}

/// Converts a string to a u64, supporting binary and hexadecimal prefixes.
fn str_to_u64(str: &str) -> Result<u64, ParseIntError> {
    const RADIXES: &[(&str, u32)] = &[("0b", 2), ("0x", 16)];
    let (str, radix) = RADIXES
        .iter()
        .find(|(prefix, _radix)| str.starts_with(prefix))
        .map(|(prefix, radix)| (&str[prefix.len()..], *radix))
        .unwrap_or((str, 10));
    // Read as a signed integer to support negative numbers
    Ok(i64::from_str_radix(str, radix)? as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn convert_str_to_u64() {
        assert_eq!(str_to_u64("101").unwrap(), 101);
        assert_eq!(str_to_u64("-101").unwrap(), -101_i64 as u64);
        assert_eq!(str_to_u64("0b101").unwrap(), 0b101);
        assert_eq!(str_to_u64("0x101").unwrap(), 0x101);
        assert!(str_to_u64("foo").is_err());
        assert!(str_to_u64("-foo").is_err());
        assert!(str_to_u64("0xfoo").is_err());
        assert!(str_to_u64("0bfoo").is_err());
    }
}
