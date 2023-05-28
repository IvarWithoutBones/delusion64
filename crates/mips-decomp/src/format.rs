pub const fn general_register_name(index: u8) -> &'static str {
    assert!(index <= 31);
    match index {
        0 => "zero",
        1 => "at",
        2 => "v0",
        3 => "v1",
        4 => "a0",
        5 => "a1",
        6 => "a2",
        7 => "a3",
        8 => "t0",
        9 => "t1",
        10 => "t2",
        11 => "t3",
        12 => "t4",
        13 => "t5",
        14 => "t6",
        15 => "t7",
        16 => "s0",
        17 => "s1",
        18 => "s2",
        19 => "s3",
        20 => "s4",
        21 => "s5",
        22 => "s6",
        23 => "s7",
        24 => "t8",
        25 => "t9",
        26 => "k0",
        27 => "k1",
        28 => "gp",
        29 => "sp",
        30 => "s8",
        31 => "ra",
        _ => unreachable!(),
    }
}

pub const fn cp0_register_name(index: u8) -> &'static str {
    match index {
        // Programmable pointer into TLB array
        0 => "Index",
        // Pseudorandom pointer into TLB array (read only)
        1 => "Random",
        // Low half of TLB entry for even virtual address (VPN)
        2 => "EntryLo0",
        // Low half of TLB entry for odd virtual address (VPN)
        3 => "EntryLo1",
        // Pointer to kernel virtual page table entry (PTE) in 32-bit mode
        4 => "Context",
        // Page size specification
        5 => "PageMask",
        // Number of wired TLB entries
        6 => "Wired",
        // for future use
        7 => "Reserved",
        // Display of virtual address that occurred an error last
        8 => "BadVAddr",
        // Timer Count
        9 => "Count",
        // High half of TLB entry (including ASID)
        10 => "EntryHi",
        // Timer Compare Value
        11 => "Compare",
        // Operation status setting
        12 => "Status",
        // Display of cause of last exception
        13 => "Cause",
        // Exception Program Counter
        14 => "EPC",
        // Processor Revision Identifier
        15 => "PRId",
        // Memory system mode setting
        16 => "Config",
        // Load Linked instruction address display
        17 => "LLAddr",
        // Memory reference trap address low bits
        18 => "WatchLo",
        // Memory reference trap address high bits
        19 => "WatchHi",
        // Pointer to Kernel virtual PTE table in 64-bit mode
        20 => "XContext",
        // Reserved for future use
        21..=25 => "Reserved",
        // Error Cache parity bits
        26 => "Parity",
        // Error Cache Error and Status register
        27 => "Cache",
        // Cache Tag register low
        28 => "TagLo",
        // Cache Tag register high
        29 => "TagHi",
        // Error Exception Program Counter
        30 => "ErrorEPC",
        // for future use
        31 => "Reserved",
        _ => unreachable!(),
    }
}
