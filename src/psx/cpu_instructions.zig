const std = @import("std");

const cpu = @import("cpu.zig");

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
            rd: cpu.RegisterName,
        },
        encoding_b: packed struct {
            imm16: u16,
        },
    },
    rt: cpu.RegisterName,
    rs: cpu.RegisterName,
    primary: PrimaryOpCode,
};

pub fn decode_instruction(op_u32: u32) Instruction {
    const op: OpCodeHelper = @bitCast(op_u32);

    const Cop0_Op = enum(u5) {
        mtc0 = 0b00100,
        _,
    };

    const Cop0 = packed struct {
        _unused: u11,
        rt: cpu.RegisterName,
        rs: cpu.RegisterName,
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

pub const Instruction = union(enum) {
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

pub const generic_rs_rt_rd = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    rd: cpu.RegisterName,
};

pub const generic_rs_rt_imm16 = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    imm16: u16,
};

pub const generic_rt_imm16 = struct {
    rt: cpu.RegisterName,
    imm16: u16,
};

pub const j = struct {
    offset: u28,
};

pub const sll = struct {
    rt: cpu.RegisterName,
    rd: cpu.RegisterName,
    shift_imm: u5,
};

pub const mtc0 = struct {
    cpu_rs: cpu.RegisterName,
    cop_rt: u5,
};

pub const add = generic_rs_rt_rd;
pub const addu = generic_rs_rt_rd;
pub const sub = generic_rs_rt_rd;
pub const subu = generic_rs_rt_rd;
pub const and_ = generic_rs_rt_rd;
pub const or_ = generic_rs_rt_rd;
pub const xor = generic_rs_rt_rd;
pub const nor = generic_rs_rt_rd;
pub const addi = generic_rs_rt_imm16;
pub const addiu = generic_rs_rt_imm16;
pub const ori = generic_rs_rt_imm16;
pub const lui = generic_rt_imm16;
pub const sw = generic_rs_rt_imm16;
