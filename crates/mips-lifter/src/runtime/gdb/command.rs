use crate::{
    runtime::{memory::tlb::AccessMode, Bus, Environment, GdbIntegration, ValidRuntime},
    target::{RegisterStorage, Target},
};
use gdbstub::outputln;
use std::{collections::HashMap, num::ParseIntError};

pub type MonitorCommandMap<'ctx, T, B> = HashMap<&'static str, MappedCommand<'ctx, T, B>>;

/// An error that can occur when handling a monitor command.
#[derive(Debug, thiserror::Error)]
pub enum MonitorCommandHandlerError {
    #[error("invalid argument '{0}'")]
    InvalidArgument(String),
    #[error("error while formatting output: {0}")]
    FormatError(#[from] std::fmt::Error),
    #[error("{0}")]
    Other(String),
}

impl From<String> for MonitorCommandHandlerError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl<'a> From<&'a str> for MonitorCommandHandlerError {
    fn from(value: &'a str) -> Self {
        Self::Other(value.to_owned())
    }
}

/// Callback to handle a monitor command.
pub type MonitorCommandHandler<This> = dyn FnMut(
    &mut This,
    &mut dyn std::fmt::Write,
    &mut dyn Iterator<Item = &str>,
) -> Result<(), MonitorCommandHandlerError>;

/// A monitor command definition.
pub struct Command<This> {
    pub name: &'static str,
    pub description: &'static str,
    pub handler: Box<MonitorCommandHandler<This>>,
}

/// A monitor command that can be mapped to either an internal or external handler.
/// This is used inside the [`MonitorCommandMap`], so that we can dispatch to the correct handler.
pub(crate) enum MappedCommand<'ctx, T: Target, B: Bus>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    Internal(Command<Environment<'ctx, T, B>>),
    External(Command<B>),
}

impl<'ctx, T: Target, B: Bus> MappedCommand<'ctx, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    /// Merges internal and external monitor commands into a single map.
    pub fn new_map(external: Vec<Command<B>>) -> MonitorCommandMap<'ctx, T, B> {
        Environment::monitor_commands()
            .into_iter()
            .map(|cmd| (cmd.name, MappedCommand::Internal(cmd)))
            .chain(
                external
                    .into_iter()
                    .map(|cmd| (cmd.name, MappedCommand::External(cmd))),
            )
            .collect()
    }

    pub fn name(&self) -> &'static str {
        match self {
            MappedCommand::Internal(cmd) => cmd.name,
            MappedCommand::External(cmd) => cmd.name,
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            MappedCommand::Internal(cmd) => cmd.description,
            MappedCommand::External(cmd) => cmd.description,
        }
    }

    pub fn handle(
        &mut self,
        env: &mut Environment<'ctx, T, B>,
        output: &mut dyn std::fmt::Write,
        args: &mut dyn Iterator<Item = &str>,
    ) -> Result<(), MonitorCommandHandlerError> {
        match self {
            MappedCommand::Internal(cmd) => (cmd.handler)(env, output, args),
            MappedCommand::External(cmd) => (cmd.handler)(&mut env.bus, output, args),
        }
    }
}

impl<T: Target, B: Bus> Environment<'_, T, B>
where
    for<'a> Environment<'a, T, B>: ValidRuntime<T>,
{
    /// The internal monitor commands, related to the CPU state.
    pub(crate) fn monitor_commands() -> Vec<Command<Self>> {
        let mut result: Vec<Command<Self>> = vec![
            Command {
                name: "help",
                description: "print this help message",
                handler: Box::new(|env, out, args| {
                    if let Some(arg) = args.next() {
                        let cmd = env
                            .debugger
                            .as_ref()
                            .unwrap()
                            .monitor_commands
                            .get(arg)
                            .ok_or_else(|| format!("unrecognized command: '{arg}'"))?;
                        writeln!(out, "{}", cmd.description())?;
                    } else {
                        for cmd in env.debugger.as_ref().unwrap().monitor_commands.values() {
                            writeln!(out, "{: <12} {}", cmd.name(), cmd.description())?;
                        }
                    }
                    Ok(())
                }),
            },
            Command {
                name: "regs",
                description: "print all the CPU registers",
                handler: Box::new(|env, out, _args| {
                    writeln!(out, "{:#?}", env.registers)?;
                    Ok(())
                }),
            },
            Command {
                name: "trace",
                description: "enable or disable instruction tracing to standard output. usage: trace [on|off]",
                handler: Box::new(|env, out, args| {
                    match args.next() {
                        Some("on") => {
                            env.trace = true;
                            writeln!(out, "tracing is now on")?;
                        },
                        Some("off") => {
                            env.trace = false;
                            writeln!(out, "tracing is now off")?;
                        },
                        None => writeln!(out, "trace is {}", if env.trace { "on" } else { "off" })?,
                        Some(arg) => Err(format!("invalid argument: {arg}. usage: trace [on|off]"))?,
                    }
                    Ok(())
                }),
            },
            Command {
                name: "paddr",
                description: "translate a virtual address to a physical address",
                handler: Box::new(|env, out, args| {
                    let vaddr = str_to_u64(args.next().ok_or("expected virtual address")?)
                        .map_err(|err| format!("invalid virtual address: {err}"))?;
                    let paddr = env.virtual_to_physical_address(vaddr, AccessMode::Read);
                    outputln!(out, "{paddr:#x}");
                    Ok(())
                }),
            },
            Command {
                name: "vaddr",
                description: "translate a physical address to a virtual address",
                handler: Box::new(|env, out, args| {
                    let paddr = str_to_u64(args.next().ok_or("expected a physical address")?)
                        .map_err(|err| format!("invalid physical address: {err}"))?;
                    let vaddr = env.physical_to_virtual_address(paddr as u32, AccessMode::Read);
                    outputln!(out, "{vaddr:#x}");
                    Ok(())
                }),
            },
            Command {
                name: "llvm-ir",
                description: "dump the LLVM IR for a given virtual address, or the program counter. usage: llvm-ir [vaddr] [path]",
                handler: Box::new(|env, out, args| {
                    let vaddr = if let Some(vaddr) = args.next() {
                        str_to_u64(vaddr).map_err(|err| format!("invalid virtual address: {err}"))?
                    } else {
                        env.registers.read_program_counter()
                    };

                    let module = env
                        .codegen
                        .labels
                        .get_containing(vaddr)
                        .next()
                        .ok_or_else(|| format!("no label at {vaddr:#x}"))?
                        .module
                        .as_ref()
                        .ok_or_else(|| format!("label at {vaddr:#x} contains no module"))?;

                    if let Some(path) = args.next() {
                        module.print_to_file(path).map_err(|err| err.to_string())?;
                        outputln!(out, "wrote LLVM IR to {path}");
                    } else {
                        let contents = module.print_to_string().to_string();
                        outputln!(out, "{contents}");
                    }
                    Ok(())
                }),
            }
        ];
        result.extend(GdbIntegration::extra_monitor_commands());
        result
    }
}

/// Converts a string to a u64, supporting binary and hexadecimal prefixes.
pub fn str_to_u64(str: &str) -> Result<u64, ParseIntError> {
    const RADIXES: &[(&str, u32)] = &[("0b", 2), ("0x", 16)];
    let (str, radix) = RADIXES
        .iter()
        .find(|(prefix, _radix)| str.starts_with(prefix))
        .map(|(prefix, radix)| (&str[prefix.len()..], *radix))
        .unwrap_or((str, 10));
    // Read as a signed integer to support negative numbers
    Ok(i128::from_str_radix(str, radix)? as u64)
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
