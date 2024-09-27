const std = @import("std");

const cpu = @import("cpu.zig");

pub fn print_instruction(op_code: u32, instruction: cpu.Instruction) void {
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

// FIXME check order
// FIXME rework this a lot
fn print_i_instruction(op_name: [:0]const u8, instruction: cpu.generic_rs_rt_imm16) void {
    std.debug.print("{s} ${}, ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.rs, instruction.imm16 });
}

fn print_r_instruction(op_name: [:0]const u8, instruction: cpu.generic_rt_imm16) void {
    std.debug.print("{s} ${}, 0x{x:0>4}\n", .{ op_name, instruction.rt, instruction.imm16 });
}

fn print_j_instruction(instruction: cpu.j) void {
    std.debug.print("j 0x{x:0>8}\n", .{instruction.offset});
}

fn print_sll_instruction(instruction: cpu.sll) void {
    if (instruction.rd == .zero and instruction.rt == .zero and instruction.shift_imm == 0) {
        std.debug.print("nop\n", .{});
    } else {
        std.debug.print("sll ${}, ${}, 0x{x:0>4}\n", .{ instruction.rd, instruction.rt, instruction.shift_imm });
    }
}

fn print_ralu_instruction(op_name: [:0]const u8, instruction: cpu.generic_rs_rt_rd) void {
    std.debug.print("{s} ${}, ${}, ${}\n", .{ op_name, instruction.rd, instruction.rt, instruction.rs });
}

fn print_cop0_instruction(instruction: cpu.mtc0) void {
    std.debug.print("mtc0 ${}, $cop0_{}\n", .{ instruction.cpu_rs, instruction.cop_rt });
}
