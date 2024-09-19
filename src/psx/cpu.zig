const std = @import("std");

// KUSEG       KSEG0     KSEG1 Length Description
// 0x00000000 0x80000000 0xa0000000 2048K Main RAM
const RAM_SizeBytes = 2 * 1024 * 1024;
const RAM_Offset = 0x00000000;
const RAM_OffsetEnd = RAM_Offset + RAM_SizeBytes;

// 0x1f000000 0x9f000000 0xbf000000 8192K Expansion Region 1
const Expansion_SizeBytes = 8 * 1024 * 1024;
const Expansion_Offset = 0x1f000000;
const Expansion_OffsetEnd = Expansion_Offset + Expansion_SizeBytes;

// 0x1f800000 0x9f800000 0xbf800000 1K Scratchpad
const Scratchpad_SizeBytes = 1024;
const Scratchpad_Offset = 0x1f800000;
const Scratchpad_OffsetEnd = Scratchpad_Offset + Scratchpad_SizeBytes;

// 0x1f801000 0x9f801000 0xbf801000 8K Hardware registers
const HWRegs_SizeBytes = 8 * 1024;
const HWRegs_Offset = 0x1f801000;
const HWRegs_OffsetEnd = HWRegs_Offset + HWRegs_SizeBytes;

// 0x1fc00000 0x9fc00000 0xbfc00000 512K BIOS ROM
pub const BIOS_SizeBytes = 512 * 1024;
const BIOS_Offset = 0x1fc00000;
const BIOS_OffsetEnd = BIOS_Offset + BIOS_SizeBytes;

const PSXState = struct {
    reg: Registers = .{},
    bios: [BIOS_SizeBytes]u8,
};

pub fn create_psx_state(bios: [BIOS_SizeBytes]u8) PSXState {
    return PSXState{
        .bios = bios,
    };
}

pub fn destroy_psx_state(psx: *PSXState) void {
    _ = psx;
}

const Registers = struct {
    pc: u32 = 0xbfc00000, // Program Counter
};

const Instruction = union(enum) {
    lui: lui,
};

const lui = struct {
    fixme: u32,
};

fn decode_instruction(op_code: u32) Instruction {
    return Instruction{ .lui = .{ .fixme = op_code } };
}

const PSXAddress = packed struct {
    offset: u29,
    mapping: enum(u3) {
        Useg = 0b000,
        Seg0 = 0b100,
        Seg1 = 0b101,
        Seg2 = 0b111,
    },
};

fn load_mem_u32(psx: *PSXState, address: PSXAddress) u32 {
    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                BIOS_Offset...BIOS_OffsetEnd - 1 => |offset| {
                    const local_offset = offset - BIOS_Offset;
                    const bios_u32_slice = psx.bios[local_offset .. local_offset + 4];
                    return std.mem.readInt(u32, bios_u32_slice[0..4], .little);
                },
                else => unreachable,
            }
        },
        .Seg2 => unreachable,
    }
}

pub fn execute(psx: *PSXState) void {
    while (true) {
        const op_code = load_mem_u32(psx, @bitCast(psx.reg.pc));

        std.debug.print("Got {x:0>8}\n", .{op_code});

        const instruction = decode_instruction(op_code);

        switch (instruction) {
            .lui => {},
        }

        psx.reg.pc +%= 4;
    }
}
