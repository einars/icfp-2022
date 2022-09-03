use blocks::*;
use egui::{Color32, Painter, Rgba, TextEdit};
use egui_extras::RetainedImage;
use parser::{BlockId, CutDirection, ProgCmd};

pub struct TemplateApp {
    action: Action,
    color: [u8; 4],

    target: RetainedImage,

    current: Painting,

    cmd_history: Vec<ProgCmd>,
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

impl TemplateApp {
    /// Called once before the first frame.
    #[allow(unused)]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customized the look at feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.
        let (size_img, pixels) =
            load_image_from_path(std::path::Path::new("../../problems/1.png")).unwrap();
        let size = (size_img[0] as u32, size_img[1] as u32);

        let data =
            egui::ColorImage::from_rgba_unmultiplied(size_img, pixels.as_flat_samples().as_slice());
        let mut img = RetainedImage::from_color_image("TODO.png", data);

        Self {
            // Example stuff:
            action: Action::CutHoriz,
            color: [0, 0, 0, 255],
            target: img,
            current: Painting::new(size),
            cmd_history: vec![],
        }
    }
}

impl eframe::App for TemplateApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    /// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let Self {
            action,
            color,
            target,
            current,
            cmd_history,
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

                ui.radio_value(action, Action::CutHoriz, "Cut Horizontally");
                ui.radio_value(action, Action::CutVert, "Cut Vertically");
                ui.radio_value(action, Action::CutPoint, "Cut Point");
                ui.radio_value(action, Action::Color, "Color");
                ui.radio_value(action, Action::Swap, "Swap");
                ui.radio_value(action, Action::Merge, "Merge");

                if *action == Action::Color {
                    ui.heading("Select Color");
                    ui.horizontal(|ui| {
                        ui.color_edit_button_srgba_unmultiplied(color);

                        color_updater(ui, color, 0);
                        color_updater(ui, color, 1);
                        color_updater(ui, color, 2);
                        color_updater(ui, color, 3);
                    });
                }

                egui::ScrollArea::vertical().show(ui, |ui| {
                    for cmd in cmd_history.iter() {
                        ui.label(cmd.to_string());
                    }
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
                canvas = target.show(ui);
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

                if *action == Action::CutHoriz {
                    paint.hline(
                        canvas.rect.min.x..=canvas.rect.max.x,
                        pos.y,
                        (2.0, Color32::RED),
                    );
                    if ctx.input().pointer.primary_clicked() {
                        if let Some(id) = pos_to_block(&canvas.rect, pos, current) {
                            let cmd = ProgCmd::LineCut(
                                id,
                                CutDirection::Y,
                                curr_pos.unwrap().1,
                            ); 
                            let _ = current
                                .apply_cmd(&cmd)
                                .unwrap();
                            cmd_history.push(cmd);
                        }
                    }
                }

                if *action == Action::CutVert {
                    paint.vline(
                        pos.x,
                        canvas.rect.min.y..=canvas.rect.max.y,
                        (2.0, Color32::RED),
                    );
                    if ctx.input().pointer.primary_clicked() {
                        if let Some(id) = pos_to_block(&canvas.rect, pos, current) {
                            let cmd = ProgCmd::LineCut(
                                id,
                                CutDirection::X,
                                curr_pos.unwrap().0,
                            );
                            let _ = current
                                .apply_cmd(&cmd)
                                .unwrap();
                            cmd_history.push(cmd);
                        }
                    }
                }

                if *action == Action::Color {
                    if ctx.input().pointer.primary_clicked() {
                        if let Some(id) = pos_to_block(&canvas.rect, pos, current) {
                            let cmd = ProgCmd::Color(id, parser::Color(*color));
                            let _ = current
                                .apply_cmd(&cmd)
                                .unwrap();
                            cmd_history.push(cmd);
                        }
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
        painting.size.1 - (pos.y - cr.min.y) as u32 - 1,
    ))
}
