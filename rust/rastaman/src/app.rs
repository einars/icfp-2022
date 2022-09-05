use crate::{DecisionTreeRasterizer, Rasterizer, UniqueColorClassifier};
use egui::{Color32, Painter, TextEdit};
use egui_extras::RetainedImage;
use smartcore::tree::decision_tree_classifier::SplitCriterion;

use crate::ImgBuf;

#[derive(Debug, PartialEq)]
enum DTCriterion {
    Gini,
    Entropy,
    ClassificationError,
}

pub struct TemplateApp {
    target: ImgBuf,
    result: Option<ImgBuf>,
    size: (u32, u32),
    criterion: DTCriterion,
    search_depth: u16,
    diameter: u32,
    save_name: String,
    score: u32,
}

impl TemplateApp {
    /// Called once before the first frame.
    #[allow(unused)]
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customized the look at feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.
        let file_name = std::env::args()
            .nth(1)
            .unwrap_or("../../problems/3.png".to_string());
        let pixels = crate::load_image_from_path(std::path::Path::new(&file_name)).unwrap();
        let size = (pixels.width(), pixels.height());

        Self {
            // Example stuff:
            target: pixels,
            result: None,
            size: size,
            criterion: DTCriterion::Entropy,
            diameter: 18,
            search_depth: 16,
            save_name: format!("{file_name}.rasta.png"),
            score: 0,
        }
    }
}

impl eframe::App for TemplateApp {
    /// Called each time the UI needs repainting, which may be many times per second.
    /// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        let Self {
            target,
            result,
            size,
            criterion,
            search_depth,
            diameter,
            save_name,
            score,
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
                ui.heading("Max score");
                ui.horizontal(|ui| {
                    ui.label("Maximum possible score: ");
                    ui.colored_label(Color32::GREEN, score.to_string());
                });
                ui.heading("Decision Tree settings");
                egui::ComboBox::from_label("Split Criterion")
                    .selected_text(format!("{:?}", criterion))
                    .show_ui(ui, |ui| {
                        ui.selectable_value(criterion, DTCriterion::Gini, "Gini");
                        ui.selectable_value(criterion, DTCriterion::Entropy, "Entropy");
                        ui.selectable_value(criterion, DTCriterion::ClassificationError, "Error");
                    });
                ui.add(egui::Slider::new(&mut *search_depth, 4..=24).text("Tree depth"));
                ui.add(egui::Slider::new(&mut *diameter, 1..=50).text("Sampling diameter"));
                ui.colored_label(Color32::GOLD, "Here be exponents!");
                if ui.button("Generate").clicked() {
                    let rasterizer =
                        DecisionTreeRasterizer::<UniqueColorClassifier>::from_image(&target)
                            .unwrap()
                            .with_depth(*search_depth)
                            .with_criterion(match criterion {
                                DTCriterion::Gini => SplitCriterion::Gini,
                                DTCriterion::Entropy => SplitCriterion::Entropy,
                                DTCriterion::ClassificationError => {
                                    SplitCriterion::ClassificationError
                                }
                            })
                            .with_diameter(*diameter);
                    *result = Some(rasterizer.rasterize(&target).unwrap());
                    if let Some(img) = result {
                    *score = compadre::compare(
                        img.as_flat_samples().as_slice(),
                        target.as_flat_samples_mut().as_slice(),
                    );
                }
                }

                if let Some(img) = result {
                    ui.heading("Save");
                    ui.text_edit_singleline(save_name);
                    if ui.button("Save").clicked() {
                        img.save(save_name).unwrap();
                    }    
                }
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            let mut result_shown = false;
            if let Some(img) = result {
                if !ctx.input().key_down(egui::Key::Space) {
                    result_shown = true;
                    RetainedImage::from_color_image(
                        "Rasterization result",
                        egui::ColorImage::from_rgba_unmultiplied(
                            [size.0 as usize, size.1 as usize],
                            img.as_flat_samples().as_slice(),
                        ),
                    )
                    .show(ui);
                }
            }
            if !result_shown {
                RetainedImage::from_color_image(
                    "Your masterpiece",
                    egui::ColorImage::from_rgba_unmultiplied(
                        [size.0 as usize, size.1 as usize],
                        target.as_flat_samples().as_slice(),
                    ),
                )
                .show(ui);
            }

            egui::TopBottomPanel::bottom("status bar").show_inside(ui, |ui| {
                ui.label("Hold down space to look at original.");
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
