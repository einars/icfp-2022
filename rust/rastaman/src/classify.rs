use std::collections::HashMap;

use super::ImgBuf;

type ColorMap = HashMap<u32, image::Rgba<u8>>;

pub trait Classifier {
    fn from_image(img: &ImgBuf) -> Self;

    fn colormap(&self) -> &ColorMap;

    /// Returned vector should be in row-major order
    fn classify(&self, img: &ImgBuf) -> Option<Vec<u32>>;
}

/// For the simplest use cases only - just use all distinct colors in the image
pub struct UniqueColorClassifier {
    color_map: ColorMap,
    u32_map: HashMap<u32, u32>,
}

impl UniqueColorClassifier {
    pub fn new(img: &ImgBuf) -> Self {
        let mut color_map: ColorMap = HashMap::new();
        let mut u32_map: HashMap<u32, u32> = HashMap::new();
        let mut color_idx = 0;
        for x in 0..img.width() {
            for y in 0..img.height() {
                let pixel = img.get_pixel(x, y);
                let u32_pixel = u32::from_ne_bytes(pixel.0);
                let color_id = if let Some(id) = u32_map.get(&u32_pixel) {
                    id.clone()
                } else {
                    u32_map.insert(u32_pixel, color_idx);
                    color_idx += 1;
                    color_idx - 1
                };

                color_map.insert(color_id, pixel.clone());
            }
        }

        Self { color_map, u32_map }
    }
}

impl Classifier for UniqueColorClassifier {
    fn from_image(img: &ImgBuf) -> Self {
        Self::new(img)
    }

    fn colormap(&self) -> &ColorMap {
        &self.color_map
    }

    fn classify(&self, img: &ImgBuf) -> Option<Vec<u32>> {
        let mut vec = Vec::with_capacity((img.width() * img.height()) as usize);
        for x in 0..img.width() {
            for y in 0..img.height() {
                let pixel_key = u32::from_ne_bytes(img.get_pixel(x, y).0);
                if let Some(color_id) = self.u32_map.get(&pixel_key) {
                    vec.push(color_id.clone());
                } else {
                    return None;
                }
            }
        }
        Some(vec)
    }
}
