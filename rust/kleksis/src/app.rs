use std::cell::Cell;

use blocks::*;
use egui::{Color32, Painter, Rgba, TextEdit};
use egui_extras::RetainedImage;
use parser::{BlockId, CutDirection, ProgCmd};

pub struct TemplateApp<'a> {
    action: Action,
    color: [u8; 4],

    target: image::ImageBuffer<image::Rgba<u8>, Vec<u8>>,

    current: Painting<'a>,

    cmd_history: Vec<ProgCmd>,

    next_cmd: Option<ProgCmd>,
    next: Painting<'a>,

    code: String,
    code_error: String,
}

#[derive(Debug, PartialEq)]
enum Action {
    CutHoriz,
    CutVert,
    CutPoint,
    Swap,
    Merge,
    Color,
}

impl<'a> TemplateApp<'a> {
    /// Called once before the first frame.
    #[allow(unused)]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customized the look at feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.
        let file_name = std::env::args()
            .nth(1)
            .expect("Specify problem file to load as cmd arg");
        let (size_img, pixels) = load_image_from_path(std::path::Path::new(&file_name)).unwrap();
        let size = (size_img[0] as u32, size_img[1] as u32);

        Self {
            // Example stuff:
            action: Action::CutHoriz,
            color: [0, 0, 0, 255],
            target: pixels,
            current: Painting::new(size),
            cmd_history: vec![],
            next_cmd: None,
            next: Painting::new(size),
            code: "".to_string(),
            code_error: "".to_string(),
        }
    }
}

impl<'a> eframe::App for TemplateApp<'a> {
    /// Called each time the UI needs repainting, which may be many times per second.
    /// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let Self {
            action,
            color,
            target,
            current,
            cmd_history,
            next_cmd,
            next,
            code,
            code_error,
        } = self;

        // Examples of how to create different panels and windows.
        // Pick whichever suits you.
        // Tip: a good default choice is to just keep the `CentralPanel`.
        // For inspiration and more examples, go to https://emilk.github.io/egui

        #[cfg(not(target_arch = "wasm32"))] // no File->Quit on web pages!
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // The top panel is often a good place for a menu bar:
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Quit").clicked() {
                        _frame.close();
                    }
                });
            });
        });

        egui::SidePanel::left("side_panel")
            .min_width(200.0)
            .show(ctx, |ui| {
                ui.heading("Action");

                ui.radio_value(action, Action::CutHoriz, "Cut Horizontally (h)");
                ui.radio_value(action, Action::CutVert, "Cut Vertically (v)");
                ui.radio_value(action, Action::CutPoint, "Cut Point (p)");
                ui.radio_value(action, Action::Color, "Color (c)");
                ui.radio_value(action, Action::Swap, "Swap (s)");
                ui.radio_value(action, Action::Merge, "Merge (m)");

                if ctx.input().key_pressed(egui::Key::H) {
                    *action = Action::CutHoriz
                }
                if ctx.input().key_pressed(egui::Key::V) {
                    *action = Action::CutVert
                }
                if ctx.input().key_pressed(egui::Key::P) {
                    *action = Action::CutPoint
                }
                if ctx.input().key_pressed(egui::Key::C) {
                    *action = Action::Color
                }
                if ctx.input().key_pressed(egui::Key::S) {
                    *action = Action::Swap
                }
                if ctx.input().key_pressed(egui::Key::M) {
                    *action = Action::Merge
                }

                if *action == Action::Color {
                    ui.heading("Select Color");
                    ui.label("Hold Ctrl to pick from image");
                    ui.horizontal(|ui| {
                        ui.color_edit_button_srgba_unmultiplied(color);

                        color_updater(ui, color, 0);
                        color_updater(ui, color, 1);
                        color_updater(ui, color, 2);
                        color_updater(ui, color, 3);
                    });
                }

                ui.heading("Scoring");
                ui.horizontal(|ui| {
                    let score = compadre::compare(
                        current.image.as_flat_samples().as_slice(),
                        target.as_flat_samples_mut().as_slice(),
                    );
                    let score_next = compadre::compare(
                        next.image.as_flat_samples().as_slice(),
                        target.as_flat_samples_mut().as_slice(),
                    );
                    ui.label("Score: ");
                    ui.label(score.to_string());
                    match score_next as i32 - score as i32 {
                        diff if diff < 0 => {
                            ui.colored_label(Color32::GREEN, diff.to_string());
                        }
                        diff if diff > 0 => {
                            ui.colored_label(Color32::RED, diff.to_string());
                        }
                        _ => (),
                    }
                });
                ui.horizontal(|ui| {
                    let mut cost = 0;
                    if let Some(cmd) = next_cmd {
                        cost = compadre::calc_cmd_score(cmd, current).unwrap_or_default();
                    }
                    ui.label("Cost: ");
                    ui.label(cost.to_string());
                });

                ui.heading("Program");
                egui::ScrollArea::vertical().show(ui, |ui| {
                    ui.horizontal(|ui| {
                        if ui.add(egui::Button::new("Apply")).clicked() {
                            *code_error = "".to_string();
                            *cmd_history = parser::source_to_tree(code);
                            match Painting::from_program(current.size, &cmd_history) {
                                Ok(prog) => *current = prog,
                                Err(err) => {
                                    *code_error = format!("Error parsing code: {err:?}");
                                }
                            };
                        }
                        if ui.add(egui::Button::new("Undo")).clicked() {
                            let _ = cmd_history.pop();
                            *code = parser::tree_to_source(cmd_history);
                            match Painting::from_program(current.size, &cmd_history) {
                                Ok(prog) => *current = prog,
                                Err(err) => {
                                    *code_error = format!("Error parsing code: {err:?}");
                                }
                            };
                        }
                        });
                    ui.colored_label(Color32::RED, code_error.clone());
                    TextEdit::multiline(code).show(ui);
                })
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            let canvas;

            if ctx.input().key_down(egui::Key::Space) {
                canvas = RetainedImage::from_color_image(
                    "Your masterpiece",
                    egui::ColorImage::from_rgba_unmultiplied(
                        [current.size.0 as usize, current.size.1 as usize],
                        current.image.as_flat_samples().as_slice(),
                    ),
                )
                .show(ui);
            } else {
                canvas = RetainedImage::from_color_image(
                    "Your masterpiece",
                    egui::ColorImage::from_rgba_unmultiplied(
                        [current.size.0 as usize, current.size.1 as usize],
                        target.as_flat_samples().as_slice(),
                    ),
                )
                .show(ui);
            }
            let paint = Painter::new(canvas.ctx.clone(), canvas.layer_id, canvas.rect);

            for block in &current.blocks {
                paint.rect_stroke(
                    block_rect(&block, current.size, &canvas.rect),
                    0.0,
                    (1.0, Color32::GRAY),
                );
            }

            let mut curr_pos = None;

            if let Some(pos) = canvas.hover_pos() {
                curr_pos = Some((
                    (pos.x - canvas.rect.min.x) as u32,
                    current.size.1 - (pos.y - canvas.rect.min.y) as u32,
                ));

                macro_rules! dispatch_cmd {
                    ($id:ident, $cmd:expr, $init:block) => {{
                        $init
                        if let Some($id) = pos_to_block(&canvas.rect, pos, current) {
                            *next_cmd =
                                Some($cmd);
                            if let Some(cmd) = next_cmd {
                                *next = current.clone();
                                if let Some(_) = next.apply_cmd(cmd).ok() {
                                    if ctx.input().pointer.primary_clicked() {
                                        current.apply_cmd(cmd).unwrap();
                                        cmd_history.push(cmd.clone());
                                        *code = parser::tree_to_source(cmd_history);
                                        *code_error = "".to_string();
                                    }
                                }
                            }
                        }
                    }};
                }

                if *action == Action::CutHoriz {
                    dispatch_cmd!(
                        id,
                        ProgCmd::LineCut(id, CutDirection::Y, curr_pos.unwrap().1),
                        {
                            paint.hline(
                                canvas.rect.min.x..=canvas.rect.max.x,
                                pos.y,
                                (2.0, Color32::RED),
                            );
                        }
                    );
                }

                if *action == Action::CutVert {
                    dispatch_cmd!(
                        id,
                        ProgCmd::LineCut(id, CutDirection::X, curr_pos.unwrap().0),
                        {
                            paint.vline(
                                pos.x,
                                canvas.rect.min.y..=canvas.rect.max.y,
                                (2.0, Color32::RED),
                            );
                        }
                    )
                }

                if *action == Action::CutPoint {
                    dispatch_cmd!(
                        id,
                        ProgCmd::PointCut(id, (curr_pos.unwrap().0, curr_pos.unwrap().1)),
                        {
                            paint.vline(
                                pos.x,
                                canvas.rect.min.y..=canvas.rect.max.y,
                                (2.0, Color32::RED),
                            );
                            paint.hline(
                                canvas.rect.min.x..=canvas.rect.max.x,
                                pos.y,
                                (2.0, Color32::RED),
                            );
                        }
                    )
                }

                if *action == Action::Color {
                    if ctx.input().modifiers.ctrl {
                        if ctx.input().pointer.primary_clicked() {
                            let x = curr_pos.unwrap().0 as i32;
                            let y = (current.size.1 - curr_pos.unwrap().1) as i32;
                            let xr =
                                (x - 2..x + 2).filter(|&x| x >= 0 && x < current.size.0 as i32);
                            let yr =
                                (y - 2..y + 2).filter(|&x| x >= 0 && x < current.size.1 as i32);
                            let mut rgba_total: [u32; 4] = [0, 0, 0, 0];
                            for x in xr.clone() {
                                for y in yr.clone() {
                                    let rgba = target.get_pixel(x as u32, y as u32).0;
                                    for i in 0..4 {
                                        rgba_total[i] += rgba[i] as u32;
                                    }
                                }
                            }

                            let num_points = xr.count() * yr.count();
                            for i in 0..4 {
                                color[i] = (rgba_total[i] / num_points as u32) as u8;
                            }
                        }
                    } else {
                        dispatch_cmd!(id, ProgCmd::Color(id, parser::Color(*color)), {});
                    }
                }
            }

            if let Some((xpos, ypos)) = curr_pos {
                ui.label(format!("Pos: ({xpos}, {ypos})"));
            }

            egui::TopBottomPanel::bottom("status bar").show_inside(ui, |ui| {
                ui.label("Hold down space to look at your own masterpiece.");
            })
        });

        if false {
            egui::Window::new("Window").show(ctx, |ui| {
                ui.label("Windows can be moved by dragging them.");
                ui.label("They are automatically sized based on contents.");
                ui.label("You can turn on resizing and scrolling if you like.");
                ui.label("You would normally chose either panels OR windows.");
            });
        }
    }
}

fn color_updater(ui: &mut egui::Ui, color: &mut [u8; 4], idx: usize) {
    let mut rtext = match color[idx] {
        0 => "".to_string(),
        _ => color[idx].to_string(),
    };
    if ui
        .add(TextEdit::singleline(&mut rtext).desired_width(25.0))
        .changed()
    {
        color[idx] = rtext.parse().unwrap_or_default();
    }
}

fn load_image_from_path(
    path: &std::path::Path,
) -> Result<([usize; 2], image::ImageBuffer<image::Rgba<u8>, Vec<u8>>), image::ImageError> {
    let image = image::io::Reader::open(path)?.decode()?;
    let size = [image.width() as _, image.height() as _];
    let image_buffer = image.to_rgba8();
    Ok((size, image_buffer))
}

fn block_rect(block: &Block, size: (u32, u32), cr: &egui::Rect) -> egui::Rect {
    egui::Rect::from_two_pos(
        egui::Pos2::new(
            block.pos.0 as f32 + cr.min.x,
            (size.1 - block.pos.1 - block.size.1) as f32 + cr.min.y,
        ),
        egui::Pos2::new(
            (block.pos.0 + block.size.0) as f32 + cr.min.x,
            (size.1 - block.pos.1) as f32 + cr.min.y,
        ),
    )
}

fn pos_to_block(cr: &egui::Rect, pos: egui::Pos2, painting: &Painting) -> Option<BlockId> {
    painting.block_by_pos((
        (pos.x - cr.min.x) as u32,
        std::cmp::max(painting.size.1 - (pos.y - cr.min.y) as u32, 1) - 1,
    ))
}
