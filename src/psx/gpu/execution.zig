const std = @import("std");

const cpu = @import("../cpu.zig");

const g0 = @import("instructions_g0.zig");
const g1 = @import("instructions_g1.zig");

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

pub fn execute_gp0_write(psx: *cpu.PSXState, command_raw: g0.CommandRaw) void {
    std.debug.print("GP0 0x{x:0>8}\n", .{@as(u32, @bitCast(command_raw))});

    switch (g0.make_command(command_raw)) {
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

pub fn execute_gp1_write(psx: *cpu.PSXState, command_raw: g1.CommandRaw) void {
    std.debug.print("GP1 COMMAND value: 0x{x:0>8}\n", .{@as(u32, @bitCast(command_raw))});

    switch (g1.make_command(command_raw)) {
        .SoftReset => |soft_reset| {
            execute_soft_reset(psx);

            std.debug.assert(soft_reset.zero_b0_23 == 0);
        },
        .SetDMADirection => |dma_direction| {
            psx.mmio.gpu.GPUSTAT.dma_direction = dma_direction.dma_direction;

            std.debug.assert(dma_direction.zero_b2_23 == 0);
        },
        .SetDisplayVRAMStart => |display_vram_start| {
            psx.gpu.display_vram_x_start = display_vram_start.x;
            psx.gpu.display_vram_y_start = display_vram_start.y;

            std.debug.assert(display_vram_start.zero_b19_23 == 0);
        },
        .SetDisplayHorizontalRange => |display_horizontal_range| {
            psx.gpu.display_horiz_start = display_horizontal_range.x1;
            psx.gpu.display_horiz_end = display_horizontal_range.x2;
        },
        .SetDisplayVerticalRange => |display_vertical_range| {
            psx.gpu.display_line_start = display_vertical_range.y1;
            psx.gpu.display_line_end = display_vertical_range.y2;

            std.debug.assert(display_vertical_range.zero_b20_23 == 0);
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
            std.debug.assert(display_mode.zero_b8_23 == 0);
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
