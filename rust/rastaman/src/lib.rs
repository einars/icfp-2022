//! Rasterize images using classified (clusterized) colors
//!
//!  # Example
//! ```
//! use rastaman::{DecisionTreeRasterizer, Rasterizer, UniqueColorClassifier};
//!
//! let img = image::io::Reader::open("../../problems/3.png").unwrap()
//!     .decode().unwrap()
//!     .to_rgba8();
//! let rasterizer = DecisionTreeRasterizer::<UniqueColorClassifier>::from_image(&img).unwrap();
//! let rasterized = rasterizer.rasterize(&img);
//! ```

pub mod app;

mod rasterize;
pub use rasterize::*;

mod classify;
pub use classify::*;

pub type ImgBuf = image::ImageBuffer<image::Rgba<u8>, Vec<u8>>;

pub fn load_image_from_path(path: &std::path::Path) -> Result<ImgBuf, image::ImageError> {
    let image = image::io::Reader::open(path)?.decode()?;
    let image_buffer = image.to_rgba8();
    Ok(image_buffer)
}
