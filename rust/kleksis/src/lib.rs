mod app;
use std::u32;

pub use app::TemplateApp;
use image::{ImageBuffer, Rgba};
use plotter::ImgBuf;

pub struct ScoreBar {
    start_score: f32,
    min_score: u32,
    min_total: u32,
    img: ImgBuf,
}

impl ScoreBar {
    pub fn new(start_score: u32) -> Self {
        ScoreBar {
            start_score: start_score as f32,
            min_score: 0,
            min_total: start_score,
            img: ImageBuffer::from_pixel(20, 400, Rgba([0, 255, 0, 255])),
        }
    }

    pub fn update(&mut self, score: u32, cost: u32) {
        if score + cost < self.min_total {
            self.min_total = score + cost;
        }
        let min_score = ((1.0 - ((score as f32 + cost as f32) / self.start_score)) * 400.0) as u32;
        if min_score > self.min_score {
            self.min_score = min_score;
        }
        let cost_at = 1.0 - (cost as f32) / self.start_score;
        let score_at = cost_at - (score as f32) / self.start_score;
        for x in 0..20 {
            for y in 0..400 {
                let pixel = match (y as f32) / 400.0 {
                    _ if y == self.min_score => Rgba([255, 255, 255, 255]),
                    at if at > cost_at => Rgba([240, 11, 50, 255]),
                    _ if score_at < 0.0 => Rgba([240, 222, 11, 255]),
                    at if at > score_at => Rgba([91, 240, 11, 255]),
                    _ => Rgba([0, 0, 0, 255]),
                };
                self.img.put_pixel(x, y, pixel);
            }
        }
    }

    pub fn get_img(&self) -> &ImgBuf {
        &self.img
    }

    pub fn get_hiscore(&self) -> u32 {
        self.min_total
    }
}
