use egui::{TextEdit, Painter, Rgba};
use egui_extras::RetainedImage;
use image::FlatSamples;
use std::io::{BufReader, Read};
use std::fs::File;

pub struct TemplateApp {
    // Example stuff:
    label: String,

    // this how you opt-out of serialization of a member
    value: f32,

    action: Action,
    color: [u8; 4],

    target: RetainedImage,
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
        let (size, pixels) = load_image_from_path(std::path::Path::new("../../problems/1.png")).unwrap();
    
        let data = egui::ColorImage::from_rgba_unmultiplied(size, pixels.as_flat_samples().as_slice());
        let mut img = RetainedImage::from_color_image("TODO.png", data);

        Self {
            // Example stuff:
            label: "Hello World!".to_owned(),
            value: 2.7,
            action: Action::CutHoriz,
            color: [0, 0, 0, 255],
            target: img,
        }
    }
}

impl eframe::App for TemplateApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    /// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let Self {
            label,
            value,
            action,
            color,
            target,
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

        egui::SidePanel::left("side_panel").min_width(200.0).show(ctx, |ui| {
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
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let canvas = target.show(ui);
            let paint = Painter::new(canvas.ctx.clone(), canvas.layer_id, canvas.rect);
            
            if *action == Action::CutHoriz {
                if let Some(pos) = canvas.hover_pos() {
                    paint.hline(canvas.rect.min.x..=canvas.rect.max.x, pos.y, (2.0, egui::Color32::RED));
                    if canvas.clicked() {
                        // todo
                    }
                }
            }
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
        _ => color[idx].to_string()
    };
    if ui.add(TextEdit::singleline(&mut rtext).desired_width(25.0)).changed() {
        color[idx] = rtext.parse().unwrap_or_default();
    }
}

fn load_image_from_path(path: &std::path::Path) -> Result<([usize;2], image::ImageBuffer<image::Rgba<u8>, Vec<u8>>), image::ImageError> {
    let image = image::io::Reader::open(path)?.decode()?;
    let size = [image.width() as _, image.height() as _];
    let image_buffer = image.to_rgba8();
    Ok((size, image_buffer))
}