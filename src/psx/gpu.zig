const std = @import("std");

const cpu = @import("cpu.zig");
const mmio_gpu = @import("mmio_gpu.zig");

pub const State = struct {
    texture_window_x_mask: u8 = 0, // FIXME Type
    texture_window_y_mask: u8 = 0, // FIXME Type
    texture_window_x_offset: u8 = 0, // FIXME Type
    texture_window_y_offset: u8 = 0, // FIXME Type

    rectangle_texture_x_flip: u1 = 0,
    rectangle_texture_y_flip: u1 = 0,

    drawing_area_left: u16 = 0, // FIXME Type
    drawing_area_top: u16 = 0, // FIXME Type
    drawing_area_right: u16 = 0, // FIXME Type
    drawing_area_bottom: u16 = 0, // FIXME Type

    drawing_x_offset: i16 = 0, // FIXME Type
    drawing_y_offset: i16 = 0, // FIXME Type

    display_vram_x_start: u16 = 0, // FIXME Type
    display_vram_y_start: u16 = 0, // FIXME Type

    display_horiz_start: u16 = 0x200, // FIXME Type
    display_horiz_end: u16 = 0xc00, // FIXME Type

    display_line_start: u16 = 0x10, // FIXME Type
    display_line_end: u16 = 0x100, // FIXME Type
};

pub fn execute_gp0_write(psx: *cpu.PSXState, command_raw: G0CommandRaw) void {
    std.debug.print("GP0 0x{x:0>8}\n", .{@as(u32, @bitCast(command_raw))});

    switch (make_gp0_command(command_raw)) {
        .Noop => {
            // Do nothing!
        },
        .ClearTextureCache => {
            unreachable;
        },
        .DrawMode => |draw_mode| {
            psx.mmio.gpu.GPUSTAT.texture_x_base = draw_mode.texture_x_base;
            psx.mmio.gpu.GPUSTAT.texture_y_base = draw_mode.texture_y_base;
            psx.mmio.gpu.GPUSTAT.semi_transparency = draw_mode.semi_transparency;
            psx.mmio.gpu.GPUSTAT.texture_page_colors = draw_mode.texture_page_colors;
            psx.mmio.gpu.GPUSTAT.dither_mode = draw_mode.dither_mode;
            psx.mmio.gpu.GPUSTAT.draw_to_display_area = draw_mode.draw_to_display_area;
            psx.mmio.gpu.GPUSTAT.texture_disable = draw_mode.texture_disable;

            psx.gpu.rectangle_texture_x_flip = draw_mode.rectangle_texture_x_flip;
            psx.gpu.rectangle_texture_y_flip = draw_mode.rectangle_texture_y_flip;

            std.debug.assert(draw_mode.texture_page_colors != .Reserved);
            std.debug.assert(draw_mode.zero_b14_23 == 0);
        },
        else => unreachable, // FIXME
    }
}

pub fn execute_gp1_write(psx: *cpu.PSXState, command_raw: G1CommandRaw) void {
    std.debug.print("GP1 COMMAND value: 0x{x:0>8}\n", .{@as(u32, @bitCast(command_raw))});

    switch (make_gp1_command(command_raw)) {
        .SoftReset => {
            execute_soft_reset(psx);
        },
        .SetDMADirection => |dma_direction| {
            psx.mmio.gpu.GPUSTAT.dma_direction = dma_direction;
        },
        .SetDisplayMode => |display_mode| {
            psx.mmio.gpu.GPUSTAT.horizontal_resolution1 = display_mode.horizontal_resolution1;
            psx.mmio.gpu.GPUSTAT.vertical_resolution = display_mode.vertical_resolution;
            psx.mmio.gpu.GPUSTAT.video_mode = display_mode.video_mode;
            psx.mmio.gpu.GPUSTAT.display_area_color_depth = display_mode.display_area_color_depth;
            psx.mmio.gpu.GPUSTAT.vertical_interlace = display_mode.vertical_interlace;
            psx.mmio.gpu.GPUSTAT.horizontal_resolution2 = display_mode.horizontal_resolution2;
            psx.mmio.gpu.GPUSTAT.reverse_flag = display_mode.reverse_flag;
            std.debug.assert(display_mode.reverse_flag == 0);
        },
        else => unreachable,
    }
}

fn execute_soft_reset(psx: *cpu.PSXState) void {
    psx.gpu = .{};
    psx.mmio.gpu.GPUSTAT = .{};
    // FIXME clear command FIFO
    // FIXME invalidate GPU cache
}

const G0OpCode = enum(u8) {
    Noop = 0x00,
    ClearTextureCache = 0x01,
    DrawMode = 0xe1,
    _,
};

const G0CommandRaw = packed struct {
    payload: u24,
    op_code: G0OpCode,
};

const G0Command = union(G0OpCode) {
    Noop,
    ClearTextureCache,
    DrawMode: packed struct {
        texture_x_base: u4,
        texture_y_base: u1,
        semi_transparency: u2,
        texture_page_colors: mmio_gpu.MMIO.Packed.TexturePageColors,
        dither_mode: u1,
        draw_to_display_area: u1,
        texture_disable: u1,
        rectangle_texture_x_flip: u1,
        rectangle_texture_y_flip: u1,
        zero_b14_23: u10,
    },
};

fn make_gp0_command(raw: G0CommandRaw) G0Command {
    return switch (raw.op_code) {
        .Noop => .{ .Noop = undefined },
        .ClearTextureCache => .{ .ClearTextureCache = undefined },
        .DrawMode => .{ .DrawMode = @bitCast(raw.payload) },
        else => unreachable,
    };
}

const G1OpCode = enum(u8) {
    SoftReset = 0x00,
    SetDMADirection = 0x04,
    SetDisplayMode = 0x08,
    _,
};

const G1CommandRaw = packed struct {
    payload: u24,
    op_code: G1OpCode,
};

const G1Command = union(G1OpCode) {
    SoftReset,
    SetDMADirection: mmio_gpu.MMIO.Packed.DMADirection,
    SetDisplayMode: packed struct {
        horizontal_resolution1: u2,
        vertical_resolution: mmio_gpu.MMIO.Packed.VerticalResolution,
        video_mode: mmio_gpu.MMIO.Packed.VideoMode,
        display_area_color_depth: mmio_gpu.MMIO.Packed.DisplayAreaColorDepth,
        vertical_interlace: u1,
        horizontal_resolution2: u1,
        reverse_flag: u1,
    },
};

fn make_gp1_command(raw: G1CommandRaw) G1Command {
    return switch (raw.op_code) {
        .SoftReset => .{ .SoftReset = undefined },
        .SetDMADirection => .{ .SetDMADirection = @enumFromInt(@as(u2, @truncate(raw.payload))) },
        .SetDisplayMode => .{ .SetDisplayMode = @bitCast(@as(u8, @truncate(raw.payload))) },
        else => unreachable,
    };
}
