use std::collections::HashMap;

use image::Rgba;

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
        let (u32_map, color_map, _) = all_color_map(img);
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

/// Retains only the most frequent colors (and merges very close colors in general)
pub struct FrequentColorClassifier {
    color_map: ColorMap,
    u32_map: HashMap<u32, u32>,
}

impl FrequentColorClassifier {
    pub fn new(img: &ImgBuf, freq_multiplier: u32, max_dist: i32) -> Self {
        let (mut u32_map, color_map, freq_map) = all_color_map(img);

        let mut cols: Vec<(u32, u32)> = freq_map
            .iter()
            .map(|(id, col)| (id.clone(), col.clone()))
            .collect();

        cols.sort_by_key(|(_, freq)| 1000000 - freq.clone());

        let mut merged = true;
        while merged {
            merged = false;
            let mut closest = None;
            let mut closest_dist = 1000000;
            if let Some((rarest_id, rarest_freq)) = cols.pop() {
                for i in 0..cols.len() {
                    let dist = color_dist_sq(&color_map, &cols[i].0, &rarest_id);
                    if rarest_freq * freq_multiplier < cols[i].1
                        || (dist < max_dist && rarest_freq * 5 < cols[i].1)
                    {
                        if dist < closest_dist {
                            closest = Some(i);
                            closest_dist = dist;
                            merged = true;
                        }
                    }
                }

                if let Some(closest_idx) = closest {
                    let (id, _) = cols[closest_idx];
                    cols[closest_idx].1 += rarest_freq;
                    let pixel_key = u32::from_ne_bytes(color_map.get(&rarest_id).unwrap().0);
                    u32_map.insert(pixel_key, id.clone());

                    cols.sort_by_key(|(_, freq)| 1000000 - freq.clone());
                }
            }
        }

        Self { color_map, u32_map }
    }
}

impl Classifier for FrequentColorClassifier {
    fn from_image(img: &ImgBuf) -> Self {
        Self::new(img, 4, 500)
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

fn all_color_map(img: &ImgBuf) -> (HashMap<u32, u32>, ColorMap, HashMap<u32, u32>) {
    let mut color_map: ColorMap = HashMap::new();
    let mut u32_map: HashMap<u32, u32> = HashMap::new();
    let mut freq_map: HashMap<u32, u32> = HashMap::new();
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

            let mut color_count = 1;
            if let Some(count) = freq_map.get(&color_id) {
                color_count = *count + 1;
            }
            freq_map.insert(color_id, color_count);
        }
    }
    (u32_map, color_map, freq_map)
}

fn color_dist_sq(map: &ColorMap, id1: &u32, id2: &u32) -> i32 {
    let col1 = map.get(id1).unwrap();
    let col2 = map.get(id2).unwrap();
    (col1[0] as i32 - col2[0] as i32) * (col1[0] as i32 - col2[0] as i32)
        + (col1[1] as i32 - col2[1] as i32) * (col1[1] as i32 - col2[1] as i32)
        + (col1[2] as i32 - col2[2] as i32) * (col1[2] as i32 - col2[2] as i32)
        + (col1[3] as i32 - col2[3] as i32) * (col1[3] as i32 - col2[3] as i32)
}
