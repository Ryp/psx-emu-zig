const std = @import("std");

const instructions = @import("cpu_instructions.zig");
const cpu = @import("cpu.zig");
const PSXState = cpu.PSXState;
const Registers = cpu.Registers;

pub fn execute_instruction(psx: *PSXState, instruction: instructions.Instruction) void {
    switch (instruction) {
        .sll => |i| execute_sll(psx, i),
        .add => |i| execute_add(psx, i),
        .addu => |i| execute_addu(psx, i),
        .sub => |i| execute_sub(psx, i),
        .subu => |i| execute_subu(psx, i),
        .and_ => |i| execute_and(psx, i),
        .or_ => |i| execute_or(psx, i),
        .xor => |i| execute_xor(psx, i),
        .nor => |i| execute_nor(psx, i),
        .slt => unreachable,
        .sltu => |i| execute_sltu(psx, i),
        .j => |i| execute_j(psx, i),
        .jal => |i| execute_jal(psx, i),
        .bne => |i| execute_bne(psx, i),
        .mtc0 => |i| execute_mtc0(psx, i),
        .addi => |i| execute_addi(psx, i),
        .addiu => |i| execute_addiu(psx, i),
        .andi => |i| execute_andi(psx, i),
        .ori => |i| execute_ori(psx, i),
        .xori => |i| execute_xori(psx, i),
        .lui => |i| execute_lui(psx, i),

        .lb => |i| execute_lb(psx, i),
        .lh => |i| execute_lh(psx, i),
        .lwl => |i| execute_lwl(psx, i),
        .lw => |i| execute_lw(psx, i),
        .sb => |i| execute_sb(psx, i),
        .sh => |i| execute_sh(psx, i),
        .swl => |i| execute_swl(psx, i),
        .sw => |i| execute_sw(psx, i),

        .invalid => unreachable,
    }
}

fn load_reg(registers: Registers, register_name: cpu.RegisterName) u32 {
    const value = switch (register_name) {
        .zero => 0,
        else => registers.r_in[@intFromEnum(register_name)],
    };

    std.debug.print("reg load 0x{x:0>8} from {}\n", .{ value, register_name });

    return value;
}

pub fn store_reg(registers: *Registers, register_name: cpu.RegisterName, value: u32) void {
    std.debug.print("reg store 0x{x:0>8} in {}\n", .{ value, register_name });

    switch (register_name) {
        .zero => {},
        else => registers.r_out[@intFromEnum(register_name)] = value,
    }
}

fn execute_sll(psx: *PSXState, instruction: instructions.sll) void {
    const reg_value = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, reg_value << instruction.shift_imm);
}

fn execute_generic_add(psx: *PSXState, lhs: u32, rhs: i32) u32 {
    _ = psx; // FIXME

    // FIXME should we support negative overflow as well?
    // NOTE: using two's-complement to ignore signedness
    const result, const overflow = @addWithOverflow(lhs, @as(u32, @bitCast(rhs)));

    if (overflow == 1) {
        // FIXME Handle overflow exception
        unreachable;
    }

    return result;
}

fn execute_generic_addu(psx: *PSXState, lhs: u32, rhs: i32) u32 {
    _ = psx; // FIXME

    return wrapping_add_u32_i32(lhs, rhs);
}

fn execute_add(psx: *PSXState, instruction: instructions.add) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result = execute_generic_add(psx, value_s, @bitCast(value_t));

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_addu(psx: *PSXState, instruction: instructions.addu) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result = execute_generic_addu(psx, value_s, @bitCast(value_t));

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_sub(psx: *PSXState, instruction: instructions.sub) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    _ = value_s;
    _ = value_t;
    unreachable;
    // store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

fn execute_subu(psx: *PSXState, instruction: instructions.subu) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    _ = value_s;
    _ = value_t;
    unreachable;
    // store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

fn execute_and(psx: *PSXState, instruction: instructions.and_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s & value_t);
}

fn execute_or(psx: *PSXState, instruction: instructions.or_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

fn execute_xor(psx: *PSXState, instruction: instructions.xor) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s ^ value_t);
}

fn execute_nor(psx: *PSXState, instruction: instructions.nor) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, ~(value_s | value_t));
}

fn execute_sltu(psx: *PSXState, instruction: instructions.nor) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    const result: u32 = if (value_s < value_t) 1 else 0;

    store_reg(&psx.registers, instruction.rd, result);
}

fn execute_j(psx: *PSXState, instruction: instructions.j) void {
    psx.registers.pc = (psx.registers.pc & 0xf0_00_00_00) | instruction.offset;
}

fn execute_jal(psx: *PSXState, instruction: instructions.j) void {
    store_reg(&psx.registers, cpu.RegisterName.ra, psx.registers.pc);

    psx.registers.pc = (psx.registers.pc & 0xf0_00_00_00) | instruction.offset;
}

fn execute_bne(psx: *PSXState, instruction: instructions.bne) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    if (value_s != value_t) {
        psx.registers.pc = wrapping_add_u32_i32(psx.registers.pc, instruction.rel_offset);
        psx.registers.pc -%= 4;
    }
}

fn execute_mtc0(psx: *PSXState, instruction: instructions.mtc0) void {
    const value = load_reg(psx.registers, instruction.cpu_rs);

    switch (instruction.target) {
        .BPC, .BDA, .Unknown, .DCIC, .BDAM, .BPCM => {
            std.debug.print("FIXME mtc0 target write ignored\n", .{});
        },
        .SR => psx.registers.sr = value,
        .CAUSE => switch (value) {
            0 => {
                std.debug.print("FIXME mtc0 target write ignored\n", .{});
            },
            else => unreachable,
        },
        else => unreachable,
    }
}

fn execute_addi(psx: *PSXState, instruction: instructions.addi) void {
    const value_s = load_reg(psx.registers, instruction.rs);

    const result = execute_generic_add(psx, value_s, @as(i16, @bitCast(instruction.imm_u16)));

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_addiu(psx: *PSXState, instruction: instructions.addiu) void {
    const value_s = load_reg(psx.registers, instruction.rs);

    const result = execute_generic_addu(psx, value_s, @as(i16, @bitCast(instruction.imm_u16)));

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_andi(psx: *PSXState, instruction: instructions.andi) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value & instruction.imm_u16);
}

fn execute_ori(psx: *PSXState, instruction: instructions.ori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value | instruction.imm_u16);
}

fn execute_xori(psx: *PSXState, instruction: instructions.xori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value ^ instruction.imm_u16);
}

fn execute_lui(psx: *PSXState, instruction: instructions.lui) void {
    const value = @as(u32, instruction.imm_u16) << 16;

    store_reg(&psx.registers, instruction.rt, value);
}

fn execute_lb(psx: *PSXState, instruction: instructions.lb) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_lh(psx: *PSXState, instruction: instructions.lh) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_lwl(psx: *PSXState, instruction: instructions.lwl) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_lw(psx: *PSXState, instruction: instructions.lw) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    const value = cpu.load_mem_u32(psx, address);

    psx.registers.pending_load = .{ .register = instruction.rt, .value = value };
}

fn execute_sb(psx: *PSXState, instruction: instructions.sb) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_sh(psx: *PSXState, instruction: instructions.sh) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    cpu.store_mem_u16(psx, address, @truncate(value));
}

fn execute_swl(psx: *PSXState, instruction: instructions.swl) void {
    _ = psx;
    _ = instruction;
    unreachable;
}

fn execute_sw(psx: *PSXState, instruction: instructions.sw) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    const address = wrapping_add_u32_i32(address_base, instruction.imm_i16);

    cpu.store_mem_u32(psx, address, value);
}

fn wrapping_add_u32_i32(lhs: u32, rhs: i32) u32 {
    // NOTE: using two's-complement to ignore signedness
    return lhs +% @as(u32, @bitCast(rhs));
}
