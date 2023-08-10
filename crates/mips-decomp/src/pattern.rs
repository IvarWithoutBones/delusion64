// The functions in this module are unfortunately all rather ugly because of the limitations of const fn's :/

use std::ops::RangeInclusive;

const MAX_COMPONENTS: usize = 16;

/// Extracts an inclusive range of bits from a number, where zero is the least significant bit.
pub const fn bit_range(num: u32, range: &RangeInclusive<u32>) -> u32 {
    assert!(*range.start() <= u32::BITS);
    assert!(*range.end() <= u32::BITS);
    let mask = 2_usize.pow(*range.end() - *range.start()) - 1;
    (num >> *range.start()) & mask as u32
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

/// Parses a binary string into a number.
const fn parse_binary_string(input: &[u8], start: usize, end: usize) -> usize {
    let mut result = 0;
    let mut i = start;
    while i < end {
        let x = input[i];
        assert!(is_binary_char(x));

        let num = x - b'0';
        result += (num as usize) << ((end - i) - 1);
        i += 1;
    }
    result
}

/// Parses a binary string into a number, returns the number of bytes parsed and the result.
const fn parse_binary(input: &[u8], start: usize) -> (usize, usize) {
    let mut end = start;
    while end < input.len() && is_binary_char(input[end]) {
        end += 1;
    }
    (end - start, parse_binary_string(input, start, end))
}

/// Counts the amount of consecutive bytes that match the given pattern.
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
            _ => panic!("invalid operand"),
        }
    }

    pub const fn is_register(&self) -> bool {
        matches!(
            self,
            Self::Source | Self::Destination | Self::Target | Self::Base
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternValue {
    Constant(u32),
    Variable(Operand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternComponent {
    pub range: RangeInclusive<u32>,
    pub value: PatternValue,
}

impl PatternComponent {
    const fn new(input: &[u8], mut start: usize) -> (usize, Self) {
        let pattern = if is_binary_char(input[start] as _) {
            let (len, num) = parse_binary(input, start);
            start += len;
            let remainder = u32::BITS - start as u32;

            PatternComponent {
                range: remainder..=(remainder + len as u32),
                value: PatternValue::Constant(num as _),
            }
        } else {
            let pattern = input[start];
            let len = count_repeated(input, start, pattern);
            let remainder = u32::BITS - start as u32;
            start += len;

            PatternComponent {
                range: (remainder - len as u32)..=remainder,
                value: PatternValue::Variable(Operand::from_char(pattern as char)),
            }
        };

        (start, pattern)
    }

    pub const fn matches(&self, num: u32) -> bool {
        match self.value {
            PatternValue::Constant(c) => bit_range(num, &self.range) == c,
            PatternValue::Variable(_) => true,
        }
    }

    pub const fn get(&self, num: u32) -> u32 {
        bit_range(num, &self.range)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstructionPattern {
    patterns: [Option<PatternComponent>; MAX_COMPONENTS],
}

impl InstructionPattern {
    const PATTERNS_INIT: Option<PatternComponent> = None;

    pub const fn new(identifier: &str) -> Self {
        let mut patterns = [Self::PATTERNS_INIT; MAX_COMPONENTS];
        let raw = remove_spaces(identifier);

        let mut start = 0;
        let mut i = 0;
        while start != u32::BITS as usize {
            let (new_start, pattern) = PatternComponent::new(&raw, start);
            patterns[i] = Some(pattern);
            start = new_start;
            i += 1;
        }

        Self { patterns }
    }

    pub const fn matches(&self, num: u32) -> bool {
        let mut i = 0;
        while i < self.patterns.len() {
            if let Some(p) = &self.patterns[i] {
                if !p.matches(num) {
                    return false;
                }
            }
            i += 1;
        }
        true
    }

    pub const fn get(&self, op: Operand, num: u32) -> Option<u32> {
        let mut i = 0;
        // Note: we cannot use `while let` here, as we want to validate the index and `slice.get()` is not const.
        while i < self.patterns.len() {
            if let Some(p) = &self.patterns[i] {
                if let PatternValue::Variable(c) = p.value {
                    if op.equals(c) {
                        return Some(p.get(num));
                    }
                }
            }
            i += 1;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_range_extract() {
        let a = 0b1010_1111;
        assert_eq!(bit_range(a, &(0..=u32::BITS)), a);
        assert_eq!(bit_range(a, &(0..=8)), 0b1010_1111);
        assert_eq!(bit_range(a, &(0..=4)), 0b1111);
        assert_eq!(bit_range(a, &(0..=3)), 0b111);
        assert_eq!(bit_range(a, &(4..=8)), 0b1010);
    }

    #[test]
    fn binary_char_validation() {
        assert!(is_binary_char(b'0'));
        assert!(is_binary_char(b'1'));
        assert!(!is_binary_char(b'2'));
    }
}
