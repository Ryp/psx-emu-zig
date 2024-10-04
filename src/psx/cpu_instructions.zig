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

pub const Instruction = union(enum) {
    sll: sll,
    jr: jr,
    jalr: jalr,
    add: add,
    addu: addu,
    sub: sub,
    subu: subu,
    and_: and_,
    or_: or_,
    xor: xor,
    nor: nor,
    slt: slt,
    sltu: sltu,
    j: j,
    jal: jal,
    beq: beq,
    bne: bne,
    blez: blez,
    bgtz: bgtz,
    mfc0: mfc0,
    mtc0: mtc0,
    addi: addi,
    addiu: addiu,
    andi: andi,
    ori: ori,
    xori: xori,
    lui: lui,
    lb: lb,
    lh: lh,
    lwl: lwl,
    lw: lw,
    lbu: lbu,
    sb: sb,
    sh: sh,
    swl: swl,
    sw: sw,
    invalid,
};

pub fn decode_instruction(op_u32: u32) Instruction {
    const op: OpCodeHelper = @bitCast(op_u32);

    return switch (op.primary) {
        .SPECIAL => switch (op.b0_15.encoding_a.secondary) {
            .SLL => .{ .sll = .{ .rt = op.rt, .rd = op.b0_15.encoding_a.rd, .shift_imm = op.b0_15.encoding_a.imm5 } },
            .SRL => unreachable,
            .SRA => unreachable,
            .SLLV => unreachable,
            .SRLV => unreachable,
            .SRAV => unreachable,

            .JR => .{ .jr = .{ .rs = op.rs } },
            .JALR => .{ .jalr = .{ .rs = op.rs, .rd = op.b0_15.encoding_a.rd } },
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

            .ADD => .{ .add = decode_generic_rs_rt_rd(op_u32) },
            .ADDU => .{ .addu = decode_generic_rs_rt_rd(op_u32) },
            .SUB => .{ .sub = decode_generic_rs_rt_rd(op_u32) },
            .SUBU => .{ .subu = decode_generic_rs_rt_rd(op_u32) },
            .AND => .{ .and_ = decode_generic_rs_rt_rd(op_u32) },
            .OR => .{ .or_ = decode_generic_rs_rt_rd(op_u32) },
            .XOR => .{ .xor = decode_generic_rs_rt_rd(op_u32) },
            .NOR => .{ .nor = decode_generic_rs_rt_rd(op_u32) },

            .SLT => .{ .slt = decode_generic_rs_rt_rd(op_u32) },
            .SLTU => .{ .sltu = decode_generic_rs_rt_rd(op_u32) },
            _ => .{ .invalid = undefined },
        },
        .BcondZ => unreachable,
        .J => .{ .j = decode_generic_j(op_u32) },
        .JAL => .{ .jal = decode_generic_j(op_u32) },
        .BEQ => .{ .beq = decode_generic_branch(op_u32) },
        .BNE => .{ .bne = decode_generic_branch(op_u32) },
        .BLEZ => .{ .blez = decode_generic_branch_2(op_u32) },
        .BGTZ => .{ .bgtz = decode_generic_branch_2(op_u32) },
        .ADDI => .{ .addi = decode_generic_rs_rt_imm_i16(op_u32) },
        .ADDIU => .{ .addiu = decode_generic_rs_rt_imm_i16(op_u32) },
        .SLTI => unreachable,
        .SLTIU => unreachable,
        .ANDI => .{ .andi = decode_generic_rs_rt_imm_u16(op_u32) },
        .ORI => .{ .ori = decode_generic_rs_rt_imm_u16(op_u32) },
        .XORI => .{ .xori = decode_generic_rs_rt_imm_u16(op_u32) },
        .LUI => .{ .lui = decode_generic_rt_imm_u16(op_u32) },
        .COP0 => decode_cop0_instruction(op_u32),
        .COP1 => unreachable,
        .COP2 => unreachable,
        .COP3 => unreachable,
        .LB => .{ .lb = decode_generic_rs_rt_imm_i16(op_u32) },
        .LH => .{ .lh = decode_generic_rs_rt_imm_i16(op_u32) },
        .LWL => .{ .lwl = decode_generic_rs_rt_imm_i16(op_u32) },
        .LW => .{ .lw = decode_generic_rs_rt_imm_i16(op_u32) },
        .LBU => .{ .lbu = decode_generic_rs_rt_imm_i16(op_u32) },
        .LHU => unreachable,
        .LWR => unreachable,
        .SB => .{ .sb = decode_generic_rs_rt_imm_i16(op_u32) },
        .SH => .{ .sh = decode_generic_rs_rt_imm_i16(op_u32) },
        .SWL => .{ .swl = decode_generic_rs_rt_imm_i16(op_u32) },
        .SW => .{ .sw = decode_generic_rs_rt_imm_i16(op_u32) },
        .SWR => unreachable,
        .LWC0 => unreachable,
        .LWC1 => unreachable,
        .LWC2 => unreachable,
        .LWC3 => unreachable,
        .SWC0 => unreachable,
        .SWC1 => unreachable,
        .SWC2 => unreachable,
        .SWC3 => unreachable,
        _ => .{ .invalid = undefined },
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

// Coprocessor Opcode/Parameter Encoding
//
//   31..26 |25..21|20..16|15..11|10..6 |  5..0  |
//    6bit  | 5bit | 5bit | 5bit | 5bit |  6bit  |
//   -------+------+------+------+------+--------+------------
//   0100nn |0|0000| rt   | rd   | N/A  | 000000 | MFCn rt,rd_dat  ;rt = dat
//   0100nn |0|0010| rt   | rd   | N/A  | 000000 | CFCn rt,rd_cnt  ;rt = cnt
//   0100nn |0|0100| rt   | rd   | N/A  | 000000 | MTCn rt,rd_dat  ;dat = rt
//   0100nn |0|0110| rt   | rd   | N/A  | 000000 | CTCn rt,rd_cnt  ;cnt = rt
//   0100nn |0|1000|00000 | <--immediate16bit--> | BCnF target ;jump if false
//   0100nn |0|1000|00001 | <--immediate16bit--> | BCnT target ;jump if true
//   0100nn |1| <--------immediate25bit--------> | COPn imm25
//   010000 |1|0000| N/A  | N/A  | N/A  | 000001 | COP0 01h  ;=TLBR   ;\if any
//   010000 |1|0000| N/A  | N/A  | N/A  | 000010 | COP0 02h  ;=TLBWI  ; (not on
//   010000 |1|0000| N/A  | N/A  | N/A  | 000110 | COP0 06h  ;=TLBWR  ; psx)
//   010000 |1|0000| N/A  | N/A  | N/A  | 001000 | COP0 08h  ;=TLBP   ;/
//   010000 |1|0000| N/A  | N/A  | N/A  | 010000 | COP0 10h  ;=RFE
//   1100nn | rs   | rt   | <--immediate16bit--> | LWCn rt_dat,[rs+imm]
//   1110nn | rs   | rt   | <--immediate16bit--> | SWCn rt_dat,[rs+imm]

const Cop0 = packed struct {
    _unused: u6,
    rd: u5,
    rt: u5,
    rs: cpu.RegisterName,
    op: Cop0_Op,
    primary: PrimaryOpCode,
};

const Cop0_Op = enum(u5) {
    mfc0 = 0b00000,
    mtc0 = 0b00100,
    _,
};

const Cop0_MTC0Target = enum(u5) {
    BPC = 3,
    BDA = 5,
    Unknown = 6,
    DCIC = 7,
    BDAM = 9,
    BPCM = 11,
    SR = 12,
    CAUSE = 13,
    _,
};

fn decode_cop0_instruction(op_u32: u32) Instruction {
    const op_cop0: Cop0 = @bitCast(op_u32);

    switch (op_cop0.op) {
        .mfc0 => return .{ .mfc0 = .{ .cpu_rs = op_cop0.rs, .target = @enumFromInt(op_cop0.rt) } },
        .mtc0 => return .{ .mtc0 = .{ .cpu_rs = op_cop0.rs, .target = @enumFromInt(op_cop0.rt) } },
        else => unreachable,
    }
}

pub const generic_rs_rt_imm_u16 = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    imm_u16: u16,
};

fn decode_generic_rs_rt_imm_u16(op_u32: u32) generic_rs_rt_imm_u16 {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rs = op.rs, .rt = op.rt, .imm_u16 = op.b0_15.encoding_b.imm16 };
}

pub const generic_rs_rt_rd = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    rd: cpu.RegisterName,
};

fn decode_generic_rs_rt_rd(op_u32: u32) generic_rs_rt_rd {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rs = op.rs, .rt = op.rt, .rd = op.b0_15.encoding_a.rd };
}

pub const generic_rs_rt_imm_i16 = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    imm_i16: i16,
};

fn decode_generic_rs_rt_imm_i16(op_u32: u32) generic_rs_rt_imm_i16 {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rs = op.rs, .rt = op.rt, .imm_i16 = @bitCast(op.b0_15.encoding_b.imm16) };
}

pub const generic_rt_imm_u16 = struct {
    rt: cpu.RegisterName,
    imm_u16: u16,
};

fn decode_generic_rt_imm_u16(op_u32: u32) generic_rt_imm_u16 {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rt = op.rt, .imm_u16 = op.b0_15.encoding_b.imm16 };
}

pub const generic_j = struct {
    offset: u28,
};

fn decode_generic_j(op_u32: u32) generic_j {
    return .{ .offset = @as(u28, @as(u26, @truncate(op_u32))) << 2 };
}

pub const j = generic_j;
pub const jal = generic_j;

pub const generic_branch = struct {
    rs: cpu.RegisterName,
    rt: cpu.RegisterName,
    rel_offset: i18,
};

fn decode_generic_branch(op_u32: u32) generic_branch {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rs = op.rs, .rt = op.rt, .rel_offset = @bitCast(@as(u18, op.b0_15.encoding_b.imm16) << 2) };
}

pub const generic_branch_2 = struct {
    rs: cpu.RegisterName,
    rel_offset: i18,
};

fn decode_generic_branch_2(op_u32: u32) generic_branch_2 {
    const op: OpCodeHelper = @bitCast(op_u32);
    return .{ .rs = op.rs, .rel_offset = @bitCast(@as(u18, op.b0_15.encoding_b.imm16) << 2) };
}

pub const beq = generic_branch;
pub const bne = generic_branch;
pub const blez = generic_branch_2;
pub const bgtz = generic_branch_2;

pub const sll = struct {
    rt: cpu.RegisterName,
    rd: cpu.RegisterName,
    shift_imm: u5,
};

pub const jr = struct {
    rs: cpu.RegisterName,
};

pub const jalr = struct {
    rs: cpu.RegisterName,
    rd: cpu.RegisterName,
};

pub const mtc0 = struct {
    cpu_rs: cpu.RegisterName,
    target: Cop0_MTC0Target,
};
pub const mfc0 = mtc0;

pub const add = generic_rs_rt_rd;
pub const addu = generic_rs_rt_rd;
pub const sub = generic_rs_rt_rd;
pub const subu = generic_rs_rt_rd;
pub const and_ = generic_rs_rt_rd;
pub const or_ = generic_rs_rt_rd;
pub const xor = generic_rs_rt_rd;
pub const nor = generic_rs_rt_rd;
pub const slt = generic_rs_rt_rd;
pub const sltu = generic_rs_rt_rd;
pub const addi = generic_rs_rt_imm_i16;
pub const addiu = generic_rs_rt_imm_i16;
pub const andi = generic_rs_rt_imm_u16;
pub const ori = generic_rs_rt_imm_u16;
pub const xori = generic_rs_rt_imm_u16;
pub const lui = generic_rt_imm_u16;
pub const lb = generic_rs_rt_imm_i16;
pub const lh = generic_rs_rt_imm_i16;
pub const lwl = generic_rs_rt_imm_i16;
pub const lw = generic_rs_rt_imm_i16;
pub const lbu = generic_rs_rt_imm_i16;
pub const sb = generic_rs_rt_imm_i16;
pub const sh = generic_rs_rt_imm_i16;
pub const swl = generic_rs_rt_imm_i16;
pub const sw = generic_rs_rt_imm_i16;
