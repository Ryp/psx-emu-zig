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
