const std = @import("std");

const instructions = @import("cpu_instructions.zig");
const cpu = @import("cpu.zig");
const PSXState = cpu.PSXState;
const Registers = cpu.Registers;

pub fn execute_instruction(psx: *PSXState, instruction: instructions.Instruction) void {
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
        .bne => |i| execute_bne(psx, i),
        .mtc0 => |i| execute_mtc0(psx, i),
        .addi => |i| execute_addi(psx, i),
        .addiu => |i| execute_addiu(psx, i),
        .ori => |i| execute_ori(psx, i),
        .lui => |i| execute_lui(psx, i),
        .lw => |i| execute_lw(psx, i),
        .sw => |i| execute_sw(psx, i),
        .invalid => unreachable,
    }
}

fn load_reg(registers: Registers, register_name: cpu.RegisterName) u32 {
    return switch (register_name) {
        .zero => 0,
        else => registers.r[@intFromEnum(register_name)],
    };
}

fn store_reg(registers: *Registers, register_name: cpu.RegisterName, value: u32) void {
    switch (register_name) {
        .zero => {},
        else => registers.r[@intFromEnum(register_name)] = value,
    }
}

fn execute_sll(psx: *PSXState, instruction: instructions.sll) void {
    const reg_value = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, reg_value << instruction.shift_imm);
}

fn execute_or(psx: *PSXState, instruction: instructions.or_) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s | value_t);
}

// FIXME
fn execute_ralu(psx: *PSXState, instruction: instructions.generic_rs_rt_rd) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    store_reg(&psx.registers, instruction.rd, value_s * value_t * 0); // FIXME

    unreachable;
}

fn execute_j(psx: *PSXState, instruction: instructions.j) void {
    psx.registers.pc = (psx.registers.pc & 0xf0_00_00_00) | instruction.offset;
}

fn execute_bne(psx: *PSXState, instruction: instructions.bne) void {
    const value_s = load_reg(psx.registers, instruction.rs);
    const value_t = load_reg(psx.registers, instruction.rt);

    if (value_s != value_t) {
        // NOTE: using two's-complement to ignore signedness
        psx.registers.pc +%= @as(u32, @bitCast(@as(i32, instruction.rel_offset)));
        psx.registers.pc -%= 4;
    }
}

fn execute_mtc0(psx: *PSXState, instruction: instructions.mtc0) void {
    const value = load_reg(psx.registers, instruction.cpu_rs);

    switch (instruction.cop_rt) {
        12 => psx.registers.sr = value,
        else => unreachable,
    }
}

fn execute_addi(psx: *PSXState, instruction: instructions.addi) void {
    const value = load_reg(psx.registers, instruction.rs);

    const result, const overflow = @addWithOverflow(value, instruction.imm_u16);

    if (overflow == 1) {
        // FIXME Handle overflow exception
        unreachable;
    }

    store_reg(&psx.registers, instruction.rt, result);
}

fn execute_addiu(psx: *PSXState, instruction: instructions.addiu) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value +% instruction.imm_u16);
}

fn execute_ori(psx: *PSXState, instruction: instructions.ori) void {
    const value = load_reg(psx.registers, instruction.rs);

    store_reg(&psx.registers, instruction.rt, value | instruction.imm_u16);
}

fn execute_lui(psx: *PSXState, instruction: instructions.lui) void {
    const value = @as(u32, instruction.imm_u16) << 16;

    store_reg(&psx.registers, instruction.rt, value);
}

fn execute_lw(psx: *PSXState, instruction: instructions.lw) void {
    const address_base = load_reg(psx.registers, instruction.rs);
    // NOTE: using two's-complement to ignore signedness
    const address = address_base +% @as(u32, @bitCast(@as(i32, instruction.imm_i16)));

    const value = cpu.load_mem_u32(psx, address);

    store_reg(&psx.registers, instruction.rt, value);
}

fn execute_sw(psx: *PSXState, instruction: instructions.sw) void {
    const value = load_reg(psx.registers, instruction.rt);

    const address_base = load_reg(psx.registers, instruction.rs);
    // NOTE: using two's-complement to ignore signedness
    const address = address_base +% @as(u32, @bitCast(@as(i32, instruction.imm_i16)));

    cpu.store_mem_u32(psx, address, value);
}
