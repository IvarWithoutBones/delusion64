use crate::{instruction::ParsedInstruction, Decompiler, MaybeInstruction, INSTRUCTION_SIZE};
use std::{fmt, ops::Range};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct JumpTarget {
    offset: usize,
    referenced_from: Vec<usize>,
}

fn generate_targets(instrs: &[MaybeInstruction]) -> Vec<JumpTarget> {
    let mut targets = Vec::new();

    let push = |targets: &mut Vec<JumpTarget>, offset: usize, from_ref: usize| {
        if let Some(instr) = instrs.get(offset) {
            if !instr.is_valid() {
                return;
            }
        } else {
            return;
        }

        if let Some(target) = targets.iter_mut().find(|b| b.offset == offset) {
            if !target.referenced_from.contains(&from_ref) {
                target.referenced_from.push(from_ref);
            }
        } else {
            let referenced_from = vec![from_ref];

            targets.push(JumpTarget {
                offset,
                referenced_from,
            });
        }
    };

    push(&mut targets, 0, 0);
    for (pos, instr) in instrs.iter().enumerate() {
        if !instr.is_valid() {
            continue;
        }

        if instr.ends_block() {
            let end_pos = if instr.has_delay_slot() { pos + 1 } else { pos };
            push(&mut targets, end_pos + 1, end_pos);
        }

        // Apply and remove the offset to account for wrapping
        let pc = (pos * INSTRUCTION_SIZE) as u64;
        if let Some(target) = instr.try_resolve_static_jump(pc) {
            let target_offset = target as usize / INSTRUCTION_SIZE;
            push(&mut targets, target_offset, pos);
        }
    }

    targets.sort();
    targets
}

#[derive(Clone, PartialEq, Eq)]
pub struct Label {
    start_offset: usize,
    pub referenced_from_offsets: Vec<usize>,
    pub instructions: Vec<ParsedInstruction>,
    pub fallthrough_offset: Option<usize>,
}

impl Label {
    pub fn set_start(&mut self, start_offset: usize) {
        self.start_offset = start_offset;
    }

    pub fn start(&self) -> usize {
        self.start_offset
    }

    pub fn end(&self) -> usize {
        self.start_offset + self.instructions.len()
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }
}

impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pos = self.start_offset * INSTRUCTION_SIZE;

        write!(f, "label_{pos:06x}:\t\t")?;
        for ref_ in &self.referenced_from_offsets {
            write!(f, " <- {:06x}", ref_ * INSTRUCTION_SIZE)?;
        }
        writeln!(f)?;

        for instr in &self.instructions {
            if let Some(target) = instr.try_resolve_static_jump(pos as u64) {
                writeln!(f, "  {pos:06x}: {instr}\t\t-> label_{target:06x}",)?;
            } else {
                writeln!(f, "  {pos:06x}: {instr}",)?;
            }

            pos += INSTRUCTION_SIZE;
        }

        if let Some(fallthrough) = self.fallthrough_offset {
            write!(f, "  -> label_{:06x}", fallthrough * INSTRUCTION_SIZE)?;
        }

        Ok(())
    }
}

fn generate_labels(instrs: &[MaybeInstruction], targets: &[JumpTarget]) -> Vec<Label> {
    let mut labels: Vec<Label> = Vec::new();
    let mut prev_fallthrough = false;

    // Ensure we dont fall through to an invalid label
    let clear_fallthrough = |labels: &mut Vec<Label>, prev_fallthrough: &mut bool| {
        if *prev_fallthrough {
            *prev_fallthrough = false;
            let prev_index = labels.len() - 1;
            labels[prev_index].fallthrough_offset = None;
        }
    };

    'outer: for target in 0..targets.len() {
        let jump_target = &targets[target];
        if jump_target.referenced_from.is_empty() {
            clear_fallthrough(&mut labels, &mut prev_fallthrough);
            continue;
        }

        let start_offset = jump_target.offset;
        let end_offset = if let Some(next) = targets.get(target + 1) {
            next.offset
        } else {
            instrs.len()
        };

        let mut instructions = Vec::new();
        for i in start_offset..end_offset {
            if let Some(instr) = instrs.get(i) {
                // Since we split blocks before at all jumps/branches getting an invalid instruction
                // in the middle of a block invalidates the entire block, theres no way not to execute it.
                if !instr.is_valid() {
                    clear_fallthrough(&mut labels, &mut prev_fallthrough);
                    continue 'outer;
                }

                instructions.push(instr.clone().unwrap());
            } else {
                clear_fallthrough(&mut labels, &mut prev_fallthrough);
                break;
            }
        }

        if instructions.is_empty() {
            clear_fallthrough(&mut labels, &mut prev_fallthrough);
            continue;
        }

        let fallthrough_offset = if let Some(instr) = instrs.get(end_offset) {
            if instr.is_valid() {
                prev_fallthrough = true;
                Some(end_offset)
            } else {
                None
            }
        } else {
            None
        };

        labels.push(Label {
            start_offset,
            referenced_from_offsets: jump_target.referenced_from.clone(),
            fallthrough_offset,
            instructions,
        });
    }

    labels
}

#[derive(PartialEq, Eq)]
#[repr(transparent)]
pub struct LabelList {
    labels: Vec<Label>,
}

impl LabelList {
    pub fn new(decomp: &Decompiler) -> Self {
        let instrs = decomp.iter().collect::<Vec<_>>();
        let targets = generate_targets(&instrs);
        let mut labels = generate_labels(&instrs, &targets);
        labels.sort_by_key(|l| l.start_offset);
        Self { labels }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Label> {
        self.labels.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Label> {
        self.labels.iter_mut()
    }

    pub fn len(&self) -> usize {
        self.labels.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn pop(&mut self) -> Option<Label> {
        self.labels.pop()
    }

    pub fn get(&self, offset: usize) -> Option<&Label> {
        self.labels.iter().find(|l| l.start_offset == offset)
    }

    pub fn set_offset(&mut self, offset: usize) {
        for label in self.iter_mut() {
            label.set_start(offset + label.start());
            for reference in label.referenced_from_offsets.iter_mut() {
                *reference += offset;
            }

            if let Some(fallthrough) = label.fallthrough_offset.as_mut() {
                *fallthrough += offset;
            }
        }
    }
}

impl From<&[u8]> for LabelList {
    fn from(bytes: &[u8]) -> Self {
        let decomp = Decompiler::from(bytes);
        Self::new(&decomp)
    }
}

impl From<LabelList> for Vec<Label> {
    fn from(labels: LabelList) -> Self {
        labels.labels
    }
}

impl fmt::Debug for LabelList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for label in &self.labels {
            writeln!(f, "{label:?}")?;
        }
        Ok(())
    }
}
