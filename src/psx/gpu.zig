const std = @import("std");

const cpu = @import("cpu.zig");
const mmio_gpu = @import("mmio_gpu.zig");

pub const State = struct {
    texture_window_x_mask: u5 = 0,
    texture_window_y_mask: u5 = 0,
    texture_window_x_offset: u5 = 0,
    texture_window_y_offset: u5 = 0,

    rectangle_texture_x_flip: u1 = 0,
    rectangle_texture_y_flip: u1 = 0,

    drawing_area_left: u10 = 0,
    drawing_area_top: u10 = 0,
    drawing_area_right: u10 = 0,
    drawing_area_bottom: u10 = 0,

    drawing_x_offset: i11 = 0,
    drawing_y_offset: i11 = 0,

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
        .SetDrawMode => |draw_mode| {
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
        .SetTextureWindow => |texture_window| {
            psx.gpu.texture_window_x_mask = texture_window.mask_x;
            psx.gpu.texture_window_y_mask = texture_window.mask_y;
            psx.gpu.texture_window_x_offset = texture_window.offset_x;
            psx.gpu.texture_window_y_offset = texture_window.offset_y;

            std.debug.assert(texture_window.zero_b20_23 == 0);
        },
        .SetDrawingAreaTopLeft => |drawing_area| {
            psx.gpu.drawing_area_left = drawing_area.left;
            psx.gpu.drawing_area_top = drawing_area.top;

            std.debug.assert(drawing_area.zero_b20_23 == 0);
        },
        .SetDrawingAreaBottomRight => |drawing_area| {
            psx.gpu.drawing_area_right = drawing_area.right;
            psx.gpu.drawing_area_bottom = drawing_area.bottom;

            std.debug.assert(drawing_area.zero_b20_23 == 0);
        },
        .SetDrawingOffset => |drawing_offset| {
            psx.gpu.drawing_x_offset = drawing_offset.x;
            psx.gpu.drawing_y_offset = drawing_offset.y;

            std.debug.assert(drawing_offset.zero_b22_23 == 0);
        },
        .SetMaskBitSetting => |mask_bit_setting| {
            psx.mmio.gpu.GPUSTAT.set_mask_when_drawing = mask_bit_setting.set_mask_when_drawing;
            psx.mmio.gpu.GPUSTAT.check_mask_before_drawing = mask_bit_setting.check_mask_before_drawing;

            std.debug.assert(mask_bit_setting.zero_b2_23 == 0);
        },
        else => unreachable,
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
    SetDrawMode = 0xe1,
    SetTextureWindow = 0xe2,
    SetDrawingAreaTopLeft = 0xe3,
    SetDrawingAreaBottomRight = 0xe4,
    SetDrawingOffset = 0xe5,
    SetMaskBitSetting = 0xe6,
    _,
};

const G0CommandRaw = packed struct {
    payload: u24,
    op_code: G0OpCode,
};

const G0Command = union(G0OpCode) {
    Noop,
    ClearTextureCache,
    SetDrawMode: packed struct {
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
    SetTextureWindow: packed struct {
        mask_x: u5,
        mask_y: u5,
        offset_x: u5,
        offset_y: u5,
        zero_b20_23: u4,
    },
    SetDrawingAreaTopLeft: packed struct {
        left: u10,
        top: u10,
        zero_b20_23: u4,
    },
    SetDrawingAreaBottomRight: packed struct {
        right: u10,
        bottom: u10,
        zero_b20_23: u4,
    },
    SetDrawingOffset: packed struct {
        x: i11,
        y: i11,
        zero_b22_23: u2,
    },
    SetMaskBitSetting: packed struct {
        set_mask_when_drawing: u1,
        check_mask_before_drawing: u1,
        zero_b2_23: u22,
    },
};

fn make_gp0_command(raw: G0CommandRaw) G0Command {
    return switch (raw.op_code) {
        .Noop => .{ .Noop = undefined },
        .ClearTextureCache => .{ .ClearTextureCache = undefined },
        .SetDrawMode => .{ .SetDrawMode = @bitCast(raw.payload) },
        .SetTextureWindow => .{ .SetTextureWindow = @bitCast(raw.payload) },
        .SetDrawingAreaTopLeft => .{ .SetDrawingAreaTopLeft = @bitCast(raw.payload) },
        .SetDrawingAreaBottomRight => .{ .SetDrawingAreaBottomRight = @bitCast(raw.payload) },
        .SetDrawingOffset => .{ .SetDrawingOffset = @bitCast(raw.payload) },
        .SetMaskBitSetting => .{ .SetMaskBitSetting = @bitCast(raw.payload) },
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
