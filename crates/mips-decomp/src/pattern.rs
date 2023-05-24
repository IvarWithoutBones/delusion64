use std::ops::Range;

const MAX_COMPONENTS: usize = 16;

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

const fn is_binary_char(c: u8) -> bool {
    c == b'0' || c == b'1'
}

const fn parse_binary_string(input: &[u8], start: usize, len: usize) -> usize {
    let mut result = 0;
    let mut i = start;
    while i < len {
        let x = input[i];
        assert!(is_binary_char(x));

        let num = x - b'0';
        result += (num as usize) << (len - i - 1);
        i += 1;
    }
    result
}

const fn bit_range(num: u32, range: &Range<u32>) -> u32 {
    assert!(range.start <= u32::BITS);
    assert!(range.end <= u32::BITS);
    let mask = 2_usize.pow(range.end - range.start) - 1;
    (num >> range.start) & mask as u32
}

const fn parse_binary(input: &[u8], start: usize) -> (usize, usize) {
    let mut i = start;
    while i < input.len() && is_binary_char(input[i]) {
        i += 1;
    }
    (i - start, parse_binary_string(input, start, i))
}

const fn parse_repeated(input: &[u8], start: usize, pattern: u8) -> usize {
    let mut i = start;
    while i < input.len() && input[i] == pattern {
        i += 1;
    }
    i - start
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            _ => panic!("invalid operand"),
        }
    }

    pub const fn is_register(&self) -> bool {
        matches!(
            self,
            Self::Source | Self::Destination | Self::Target | Self::Base
        )
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
    pub range: Range<u32>,
    pub value: PatternValue,
}

impl PatternComponent {
    const fn new(input: &[u8], mut start: usize) -> (usize, Self) {
        let pattern = if is_binary_char(input[start] as _) {
            let (len, num) = parse_binary(input, start);
            start += len;
            let remainder = u32::BITS - start as u32;

            PatternComponent {
                range: remainder..remainder + len as u32,
                value: PatternValue::Constant(num as _),
            }
        } else {
            let pattern = input[start];
            let len = parse_repeated(input, start, pattern);
            let remainder = u32::BITS - start as u32;
            start += len;

            PatternComponent {
                range: (remainder - len as u32)..remainder,
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

    pub fn matches(&self, num: u32) -> bool {
        self.iter().all(|p| p.matches(num))
    }

    pub fn get(&self, op: Operand, num: u32) -> Option<u32> {
        Some(
            self.iter()
                .find(|p| {
                    if let PatternValue::Variable(c) = p.value {
                        c == op
                    } else {
                        false
                    }
                })?
                .get(num),
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = &PatternComponent> {
        self.patterns
            .iter()
            .take_while(|p| p.is_some())
            .map(|p| p.as_ref().unwrap())
    }
}
