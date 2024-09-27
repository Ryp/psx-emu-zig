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
    registers: Registers = .{},
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

// Register Name Conventional use
const RegisterName = enum(u5) {
    zero = 0, // Always zero
    at = 1, // Assembler temporary
    v0 = 2, // Function return values
    v1 = 3,
    a0 = 4, // Function arguments
    a1,
    a2,
    a3,
    t0 = 8, // Temporary registers
    t1,
    t2,
    t3,
    t4,
    t5,
    t6,
    t7,
    s0 = 16, // Saved registers
    s1,
    s2,
    s3,
    s4,
    s5,
    s6,
    s7,
    t8 = 24, // Temporary registers
    t9 = 25,
    k0 = 26, // Kernel reserved registers
    k1 = 27,
    gp = 28, // Global pointer
    sp = 29, // Stack pointer
    fp = 30, // Frame pointer
    ra = 31, // Function return address
};

const Registers = struct {
    pc: u32 = 0xbfc00000, // Program Counter
    r: [32]u32 = undefined, // FIXME does it have an initial value?
    sr: u32 = undefined,
};

fn load_reg(registers: Registers, register_name: RegisterName) u32 {
    return switch (register_name) {
        .zero => 0,
        else => registers.r[@intFromEnum(register_name)],
    };
}

fn store_reg(registers: *Registers, register_name: RegisterName, value: u32) void {
    switch (register_name) {
        .zero => {},
        else => registers.r[@intFromEnum(register_name)] = value,
    }
}

const PrimaryOpCode = enum(u6) {
    SPECIAL = 0x00,
    BcondZ = 0x01,
    J = 0x02,
    JAL = 0x03,
    BEQ = 0x04,
    BNE = 0x05,
    BLEZ = 0x06,
    BGTZ = 0x07,
    ADDI = 0x08,
    ADDIU = 0x09,
    SLTI = 0x0A,
    SLTIU = 0x0B,
    ANDI = 0x0C,
    ORI = 0x0D,
    XORI = 0x0E,
    LUI = 0x0F,
    COP0 = 0x10,
    COP1 = 0x11,
    COP2 = 0x12,
    COP3 = 0x13,

    LB = 0x20,
    LH = 0x21,
    LWL = 0x22,
    LW = 0x23,
    LBU = 0x24,
    LHU = 0x25,
    LWR = 0x26,

    SB = 0x28,
    SH = 0x29,
    SWL = 0x2A,
    SW = 0x2B,

    SWR = 0x2E,

    LWC0 = 0x30,
    LWC1 = 0x31,
    LWC2 = 0x32,
    LWC3 = 0x33,

    SWC0 = 0x38,
    SWC1 = 0x39,
    SWC2 = 0x3A,
    SWC3 = 0x3B,
    _,
};

const SecondaryOpCode = enum(u6) {
    SLL = 0x00,

    SRL = 0x02,
    SRA = 0x03,
    SLLV = 0x04,

    SRLV = 0x06,
    SRAV = 0x07,
    JR = 0x08,
    JALR = 0x09,

    SYSCALL = 0x0C,
    BREAK = 0x0D,

    MFHI = 0x10,
    MTHI = 0x11,
    MFLO = 0x12,
    MTLO = 0x13,

    MULT = 0x18,
    MULTU = 0x19,
    DIV = 0x1A,
    DIVU = 0x1B,

    ADD = 0x20,
    ADDU = 0x21,
    SUB = 0x22,
    SUBU = 0x23,
    AND = 0x24,
    OR = 0x25,
    XOR = 0x26,
    NOR = 0x27,

    SLT = 0x2A,
    SLTU = 0x2B,
    _,
};

// Opcode/Parameter Encoding
//
//   31..26 |25..21|20..16|15..11|10..6 |  5..0  |
//    6bit  | 5bit | 5bit | 5bit | 5bit |  6bit  |
//   -------+------+------+------+------+--------+------------
//   000000 | N/A  | rt   | rd   | imm5 | 0000xx | shift-imm
//   000000 | rs   | rt   | rd   | N/A  | 0001xx | shift-reg
//   000000 | rs   | N/A  | N/A  | N/A  | 001000 | jr
//   000000 | rs   | N/A  | rd   | N/A  | 001001 | jalr
//   000000 | <-----comment20bit------> | 00110x | sys/brk
//   000000 | N/A  | N/A  | rd   | N/A  | 0100x0 | mfhi/mflo
//   000000 | rs   | N/A  | N/A  | N/A  | 0100x1 | mthi/mtlo
//   000000 | rs   | rt   | N/A  | N/A  | 0110xx | mul/div
//   000000 | rs   | rt   | rd   | N/A  | 10xxxx | alu-reg
//   000001 | rs   | 00000| <--immediate16bit--> | bltz
//   000001 | rs   | 00001| <--immediate16bit--> | bgez
//   000001 | rs   | 10000| <--immediate16bit--> | bltzal
//   000001 | rs   | 10001| <--immediate16bit--> | bgezal
//   000001 | rs   | xxxx0| <--immediate16bit--> | bltz  ;\undocumented dupes
//   000001 | rs   | xxxx1| <--immediate16bit--> | bgez  ;/(when bit17-19=nonzero)
//   00001x | <---------immediate26bit---------> | j/jal
//   00010x | rs   | rt   | <--immediate16bit--> | beq/bne
//   00011x | rs   | N/A  | <--immediate16bit--> | blez/bgtz
//   001xxx | rs   | rt   | <--immediate16bit--> | alu-imm
//   001111 | N/A  | rt   | <--immediate16bit--> | lui-imm
//   100xxx | rs   | rt   | <--immediate16bit--> | load rt,[rs+imm]
//   101xxx | rs   | rt   | <--immediate16bit--> | store rt,[rs+imm]
//   x1xxxx | <------coprocessor specific------> | coprocessor (see below)
const OpCodeHelper = packed struct {
    b0_15: packed union {
        encoding_a: packed struct {
            secondary: SecondaryOpCode,
            imm5: u5,
            rd: RegisterName,
        },
        encoding_b: packed struct {
            imm16: u16,
        },
    },
    rt: RegisterName,
    rs: RegisterName,
    primary: PrimaryOpCode,
};

const Instruction = union(enum) {
    sll: sll,
    add: add,
    addu: addu,
    sub: sub,
    subu: subu,
    and_: and_,
    or_: or_,
    xor: xor,
    nor: nor,
    j: j,
    mtc0: mtc0,
    addi: addi,
    addiu: addiu,
    ori: ori,
    lui: lui,
    sw: sw,
    invalid,
};

const generic_rs_rt_rd = struct {
    rs: RegisterName,
    rt: RegisterName,
    rd: RegisterName,
};

const generic_rs_rt_imm16 = struct {
    rs: RegisterName,
    rt: RegisterName,
    imm16: u16,
};

const generic_rt_imm16 = struct {
    rt: RegisterName,
    imm16: u16,
};

const j = struct {
    offset: u28,
};

const sll = struct {
    rt: RegisterName,
    rd: RegisterName,
    shift_imm: u5,
};

const mtc0 = struct {
    cpu_rs: RegisterName,
    cop_rt: u5,
};

const add = generic_rs_rt_rd;
const addu = generic_rs_rt_rd;
const sub = generic_rs_rt_rd;
const subu = generic_rs_rt_rd;
const and_ = generic_rs_rt_rd;
const or_ = generic_rs_rt_rd;
const xor = generic_rs_rt_rd;
const nor = generic_rs_rt_rd;
const addi = generic_rs_rt_imm16;
const addiu = generic_rs_rt_imm16;
const ori = generic_rs_rt_imm16;
const lui = generic_rt_imm16;
const sw = generic_rs_rt_imm16;

fn decode_instruction(op_u32: u32) Instruction {
    const op: OpCodeHelper = @bitCast(op_u32);

    const Cop0_Op = enum(u5) {
        mtc0 = 0b00100,
        _,
    };

    const Cop0 = packed struct {
        _unused: u11,
        rt: RegisterName,
        rs: RegisterName,
        op: Cop0_Op,
        primary: PrimaryOpCode,
    };
    const op_cop0: Cop0 = @bitCast(op_u32);

    return switch (op.primary) {
        .SPECIAL => switch (op.b0_15.encoding_a.secondary) {
            .SLL => .{ .sll = .{ .rt = op.rt, .rd = op.b0_15.encoding_a.rd, .shift_imm = op.b0_15.encoding_a.imm5 } },
            .SRL => unreachable,
            .SRA => unreachable,
            .SLLV => unreachable,
            .SRLV => unreachable,
            .SRAV => unreachable,

            .JR => unreachable,
            .JALR => unreachable,
            .SYSCALL => unreachable,
            .BREAK => unreachable,

            .MFHI => unreachable,
            .MTHI => unreachable,
            .MFLO => unreachable,
            .MTLO => unreachable,

            .MULT => unreachable,
            .MULTU => unreachable,
            .DIV => unreachable,
            .DIVU => unreachable,

            .ADD => .{ .add = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .ADDU => .{ .addu = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .SUB => .{ .sub = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .SUBU => .{ .subu = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .AND => .{ .and_ = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .OR => .{ .or_ = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .XOR => .{ .xor = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },
            .NOR => .{ .nor = .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd } },

            .SLT => unreachable,
            .SLTU => unreachable,
            else => unreachable,
        },
        .BcondZ => unreachable,
        .J => .{ .j = .{ .offset = @as(u28, @as(u26, @truncate(op_u32))) << 2 } },
        .JAL => unreachable,
        .BEQ => unreachable,
        .BNE => unreachable,
        .BLEZ => unreachable,
        .BGTZ => unreachable,
        .ADDI => .{ .addi = .{ .rs = op.rs, .rt = op.rt, .imm16 = op.b0_15.encoding_b.imm16 } },
        .ADDIU => .{ .addiu = .{ .rs = op.rs, .rt = op.rt, .imm16 = op.b0_15.encoding_b.imm16 } },
        .SLTI => unreachable,
        .SLTIU => unreachable,
        .ANDI => unreachable,
        .ORI => .{ .ori = .{ .rs = op.rs, .rt = op.rt, .imm16 = op.b0_15.encoding_b.imm16 } },
        .XORI => unreachable,
        .LUI => .{ .lui = .{ .rt = op.rt, .imm16 = op.b0_15.encoding_b.imm16 } },
        .COP0 => switch (op_cop0.op) {
            .mtc0 => .{ .mtc0 = .{ .cpu_rs = op_cop0.rs, .cop_rt = @intFromEnum(op_cop0.rt) } },
            else => unreachable,
        },
        .COP1 => unreachable,
        .COP2 => unreachable,
        .COP3 => unreachable,
        .LB => unreachable,
        .LH => unreachable,
        .LWL => unreachable,
        .LW => unreachable,
        .LBU => unreachable,
        .LHU => unreachable,
        .LWR => unreachable,
        .SB => unreachable,
        .SH => unreachable,
        .SWL => unreachable,
        .SW => .{ .sw = .{ .rs = op.rs, .rt = op.rt, .imm16 = op.b0_15.encoding_b.imm16 } },
        .SWR => unreachable,
        .LWC0 => unreachable,
        .LWC1 => unreachable,
        .LWC2 => unreachable,
        .LWC3 => unreachable,
        .SWC0 => unreachable,
        .SWC1 => unreachable,
        .SWC2 => unreachable,
        .SWC3 => unreachable,
        else => unreachable,
    };
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

fn load_mem_u32(psx: *PSXState, address_u32: u32) u32 {
    std.debug.print("load addr: 0x{x:0>8}\n", .{address_u32});

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 4 == 0); // FIXME implement bus errors

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

fn store_mem_u32(psx: *PSXState, address_u32: u32, value: u32) void {
    std.debug.print("store addr: 0x{x:0>8}\n", .{address_u32});
    std.debug.print("store value: 0x{x:0>8}\n", .{value});

    if (psx.registers.sr & 0x00010000 != 0) {
        std.debug.print("FIXME store ignored because of cache isolation\n", .{});
        return;
    }

    const address: PSXAddress = @bitCast(address_u32);

    std.debug.assert(address.offset % 4 == 0); // FIXME implement bus errors

    switch (address.mapping) {
        .Useg, .Seg0, .Seg1 => {
            switch (address.offset) {
                HWRegs_Offset...HWRegs_OffsetEnd - 1 => |offset| {
                    const local_offset: u13 = @intCast(offset - HWRegs_Offset);
                    const local_offset_typed: HWRegOffsets = @enumFromInt(local_offset);
                    switch (local_offset_typed) {
                        .Expansion1BaseAddress, .Expansion2BaseAddress => {
                            std.debug.print("FIXME store ignored to expansion register 0x{x:0>8}\n", .{local_offset});
                        },
                        else => {
                            std.debug.print("FIXME store ignored at local offset 0x{x:0>8}\n", .{local_offset});
                        },
                    }
                },
                BIOS_Offset...BIOS_OffsetEnd - 1 => unreachable, // This should be read-only
                else => unreachable,
            }
        },
        .Seg2 => {
            switch (address.offset) {
                0x1ffe0130 => {
                    std.debug.print("FIXME store ignored at offset 0x{x:0>8}\n", .{address_u32});
                },
                else => unreachable,
            }
        },
    }
}

fn execute_instruction(psx: *PSXState, instruction: Instruction) void {
    switch (instruction) {
        .sll => |i| execute_sll(psx, i),
        .add => |i| execute_ralu(psx, i),
        .addu => |i| execute_ralu(psx, i),
        .sub => |i| execute_ralu(psx, i),
        .subu => |i| execute_ralu(psx, i),
        .and_ => |i| execute_ralu(psx, i),
        .or_ => |i| execute_or(psx, i),
        .xor => |i| execute_ralu(psx, i),
        .nor => |i| execute_ralu(psx, i),
        .j => |i| execute_j(psx, i),
        .mtc0 => |i| execute_mtc0(psx, i),
        .addi => |i| execute_addi(psx, i),
        .addiu => |i| execute_addiu(psx, i),
        .ori => |i| execute_ori(psx, i),
        .lui => |i| execute_lui(psx, i),
        .sw => |i| execute_sw(psx, i),
        .invalid => unreachable,
    }
}

fn execute_sll(psx: *PSXState, instruction: sll) void {
    const reg_value = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, reg_value << instruction.shift_imm);
}

fn execute_or(psx: *PSXState, instruction: or_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

// FIXME
fn execute_ralu(psx: *PSXState, instruction: generic_rs_rt_rd) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s * value_t * 0); // FIXME

    unreachable;
}

fn execute_j(psx: *PSXState, instruction: j) void {
    psx.registers.pc = (psx.registers.pc & 0xf0_00_00_00) | instruction.offset;
}

fn execute_mtc0(psx: *PSXState, instruction: mtc0) void {
    const value = load_reg(psx.registers, instruction.cpu_rs);

    switch (instruction.cop_rt) {
        12 => psx.registers.sr = value,
        else => unreachable,
    }
}

fn execute_addi(psx: *PSXState, instruction: addi) void {
    const value = load_reg(psx.registers, instruction.rs);

    const result, const overflow = @addWithOverflow(value, instruction.imm16);

    // FIXME Handle overflow exception
    _ = overflow;

    store_reg(&psx.registers, instruction.rt, result);

    unreachable;
}

fn execute_addiu(psx: *PSXState, instruction: addiu) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value +% instruction.imm16);
}

fn execute_ori(psx: *PSXState, instruction: ori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value | instruction.imm16);
}

fn execute_lui(psx: *PSXState, instruction: lui) void {
    const value = @as(u32, instruction.imm16) << 16;

    store_reg(&psx.registers, instruction.rt, value);
}

fn execute_sw(psx: *PSXState, instruction: sw) void {
    const value = load_reg(psx.registers, instruction.rt);
    const address_base = load_reg(psx.registers, instruction.rs);
    const address_offset_signed: i16 = @bitCast(instruction.imm16);
    // NOTE: using two's-complement to ignore signedness
    const address = address_base +% @as(u32, @bitCast(@as(i32, address_offset_signed)));

    store_mem_u32(psx, address, value);
}

pub fn execute(psx: *PSXState) void {
    const nop = decode_instruction(0);
    var next_instruction = nop;
    var next_op_code: u32 = 0;

    while (true) {
        const instruction = next_instruction;
        const op_code = next_op_code;

        next_op_code = load_mem_u32(psx, psx.registers.pc);

        next_instruction = decode_instruction(next_op_code);

        // Execution is pipelined so we increment the PC before even starting to execute the current instruction
        psx.registers.pc +%= 4;

        print_instruction(op_code, instruction);

        execute_instruction(psx, instruction);
    }
}

// FIXME check order
// FIXME rework this a lot
fn print_i_instruction(op_name: [:0]const u8, instruction: generic_rs_rt_imm16) void {
    std.debug.print("{s} ${}, ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.rs, instruction.imm16 });
}

fn print_r_instruction(op_name: [:0]const u8, instruction: generic_rt_imm16) void {
    std.debug.print("{s} ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.imm16 });
}

fn print_j_instruction(instruction: j) void {
    std.debug.print("j 0x{x:0>8}\n", .{instruction.offset});
}

fn print_sll_instruction(instruction: sll) void {
    if (instruction.rd == .zero and instruction.rt == .zero and instruction.shift_imm == 0) {
        std.debug.print("nop\n", .{});
    } else {
        std.debug.print("sll ${}, ${}, 0x{x:0>4}\n", .{ instruction.rd, instruction.rt, instruction.shift_imm });
    }
}

fn print_ralu_instruction(op_name: [:0]const u8, instruction: generic_rs_rt_rd) void {
    std.debug.print("{s} ${}, ${}, ${}\n", .{ op_name, instruction.rd, instruction.rt, instruction.rs });
}

fn print_cop0_instruction(instruction: mtc0) void {
    std.debug.print("mtc0 ${}, $cop0_{}\n", .{ instruction.cpu_rs, instruction.cop_rt });
}

fn print_instruction(op_code: u32, instruction: Instruction) void {
    std.debug.print("0b{b:0>32} 0x{x:0>8} ", .{ op_code, op_code });

    switch (instruction) {
        .sll => |i| print_sll_instruction(i),

        .add => |i| print_ralu_instruction("add", i),
        .addu => |i| print_ralu_instruction("addu", i),
        .sub => |i| print_ralu_instruction("sub", i),
        .subu => |i| print_ralu_instruction("subu", i),
        .and_ => |i| print_ralu_instruction("and", i),
        .or_ => |i| print_ralu_instruction("or", i),
        .xor => |i| print_ralu_instruction("xor", i),
        .nor => |i| print_ralu_instruction("nor", i),

        .j => |i| print_j_instruction(i),
        .mtc0 => |i| print_cop0_instruction(i),
        .addi => |i| print_i_instruction("addi", i),
        .addiu => |i| print_i_instruction("addiu", i),
        .ori => |i| print_i_instruction("ori", i),
        .lui => |i| print_r_instruction("lui", i),
        .sw => |i| print_i_instruction("sw", i),
        .invalid => unreachable,
    }
}

const HWRegOffsets = enum(u13) {
    Expansion1BaseAddress = 0,
    Expansion2BaseAddress = 4,
    _,
};
