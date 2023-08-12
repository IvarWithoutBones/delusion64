// The functions in this module are unfortunately all rather ugly because of the limitations of const fn's :/

use std::ops::RangeInclusive;

const MAX_COMPONENTS: usize = 16;

/// Counts the amount of bits in a range, and returns a mask which covers those bits.
const fn bits_in_range(range: &RangeInclusive<u32>) -> u32 {
    (2_usize.pow(*range.end() - *range.start()) - 1) as u32
}

/// Checks if a number is a binary digit (1 or 0), in ASCII.
const fn is_binary_char(c: u8) -> bool {
    c == b'0' || c == b'1'
}

/// Removes all ASCII spaces from a string.
const fn remove_spaces(s: &str) -> [u8; u32::BITS as usize] {
    let raw = s.as_bytes();
    let mut result = [0; u32::BITS as usize];

    let mut i = 0;
    let mut j = 0;
    while i < raw.len() {
        if raw[i] != b' ' {
            result[j] = raw[i];
            j += 1;
        }
        i += 1;
    }
    result
}

/// Parses a binary string into a number, returns the number of bytes parsed and the result.
const fn parse_binary(input: &[u8], start: usize) -> (usize, usize) {
    // Count the number of consecutive binary digits.
    let mut end = start;
    while end < input.len() && is_binary_char(input[end]) {
        end += 1;
    }

    // Parse the binary digits into a number.
    let mut result = 0;
    let mut i = start;
    while i < end {
        let x = input[i];
        result += ((x - b'0') as usize) << ((end - i) - 1);
        i += 1;
    }

    (end - start, result)
}

/// Counts the amount of consecutive bytes that match the given character.
const fn count_repeated(input: &[u8], start: usize, pattern: u8) -> usize {
    let mut i = start;
    while i < input.len() && input[i] == pattern {
        i += 1;
    }
    i - start
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operand {
    Source,
    Destination,
    Target,
    Immediate,
    Base,
    Offset,
    Coprocessor,
    Format,
    Condition,
    CacheSubject,
    CacheOpcode,
    FloatDestination,
    FloatSource,
    FloatTarget,
}

impl Operand {
    // Workaround for const fn's not being allowed in traits yet (From<char>)
    pub const fn from_char(value: char) -> Self {
        match value {
            's' => Self::Source,
            'd' => Self::Destination,
            't' => Self::Target,
            'k' => Self::Immediate,
            'b' => Self::Base,
            'f' => Self::Offset,
            'x' => Self::Coprocessor,
            'a' => Self::Format,
            'c' => Self::Condition,
            'y' => Self::CacheOpcode,
            'j' => Self::CacheSubject,
            'S' => Self::FloatSource,
            'D' => Self::FloatDestination,
            'T' => Self::FloatTarget,
            _ => panic!("Invalid operand"),
        }
    }

    pub const fn is_general_purpose_register(&self) -> bool {
        matches!(
            self,
            Self::Source | Self::Destination | Self::Target | Self::Base
        )
    }

    pub const fn is_fpu_register(&self) -> bool {
        matches!(
            self,
            Self::FloatSource | Self::FloatDestination | Self::FloatTarget
        )
    }

    // Questionable workaround for the equality operator not being allowed in const fn's.
    // See https://github.com/rust-lang/rust/issues/92827.
    pub const fn equals(self, other: Self) -> bool {
        self as usize == other as usize
    }
}

impl From<char> for Operand {
    fn from(value: char) -> Self {
        Self::from_char(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PatternValue {
    Constant(u32),
    Variable(Operand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternComponent {
    /// The value of this pattern component.
    pub value: PatternValue,
    /// The range of bits that this pattern component occupies.
    pub range: RangeInclusive<u32>,
    /// A mask that covers the range of bits that this pattern component occupies.
    mask: u32,
}

impl PatternComponent {
    const fn new(input: &[u8], mut start: usize) -> (usize, Self) {
        let pattern = if is_binary_char(input[start] as _) {
            let (len, num) = parse_binary(input, start);
            start += len;
            let remainder = u32::BITS - start as u32;

            let range = remainder..=(remainder + len as u32);
            PatternComponent {
                value: PatternValue::Constant(num as _),
                mask: bits_in_range(&range),
                range,
            }
        } else {
            let pattern = input[start];
            let len = count_repeated(input, start, pattern);
            let remainder = u32::BITS - start as u32;
            start += len;

            let range = (remainder - len as u32)..=remainder;
            PatternComponent {
                value: PatternValue::Variable(Operand::from_char(pattern as char)),
                mask: bits_in_range(&range),
                range,
            }
        };

        (start, pattern)
    }

    #[inline]
    pub const fn matches(&self, num: u32) -> bool {
        match self.value {
            PatternValue::Constant(c) => self.extract_from(num) == c,
            PatternValue::Variable(_) => true,
        }
    }

    #[inline]
    pub const fn extract_from(&self, num: u32) -> u32 {
        (num >> *self.range.start()) & self.mask
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionPattern {
    constant_patterns: [Option<PatternComponent>; MAX_COMPONENTS],
    variable_patterns: [Option<PatternComponent>; MAX_COMPONENTS],
}

impl InstructionPattern {
    const PATTERNS_INIT: Option<PatternComponent> = None;

    pub const fn new(identifier: &str) -> Self {
        let mut constant_patterns = [Self::PATTERNS_INIT; MAX_COMPONENTS];
        let mut variable_patterns = [Self::PATTERNS_INIT; MAX_COMPONENTS];

        let raw = remove_spaces(identifier);
        let mut start = 0;
        let mut i = 0;
        while start != u32::BITS as usize {
            let (new_start, pattern) = PatternComponent::new(&raw, start);
            start = new_start;

            match pattern.value {
                PatternValue::Constant(_) => {
                    constant_patterns[i] = Some(pattern);
                    i += 1;
                }
                PatternValue::Variable(v) => variable_patterns[v as usize] = Some(pattern),
            }
        }

        Self {
            constant_patterns,
            variable_patterns,
        }
    }

    pub const fn matches(&self, num: u32) -> bool {
        let mut i = 0;
        while i < self.constant_patterns.len() {
            if let Some(p) = &self.constant_patterns[i] {
                if !p.matches(num) {
                    return false;
                }
            } else {
                break;
            }
            i += 1;
        }
        true
    }

    pub const fn get(&self, op: Operand, num: u32) -> Option<u32> {
        if let Some(p) = &self.variable_patterns[op as usize] {
            Some(p.extract_from(num))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_char_validation() {
        assert!(is_binary_char(b'0'));
        assert!(is_binary_char(b'1'));
        assert!(!is_binary_char(b'2'));
    }
}
