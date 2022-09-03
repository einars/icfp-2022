//! Salīdzina divus png
//!
//! by convention, 400x400x4
//!
//! kā helperis, ir pngread f-ja:
//!
//! ```
//! use compadre::{pngread, compare};
//!
//! let ia = pngread("./test.png");
//! let ib = pngread("./test.png");
//!
//! assert_eq!(compare(&ia, &ib), 0);
//! ```

use std::fs::File;

type Rgb = [u8;4];

fn compare_rgb(a: Rgb, b:Rgb) -> f64 {

    let a0:i32 = (a[0] as i32) - (b[0] as i32);
    let a1:i32 = (a[1] as i32) - (b[1] as i32);
    let a2:i32 = (a[2] as i32) - (b[2] as i32);
    let a3:i32 = (a[3] as i32) - (b[3] as i32);

    ((a0.pow(2) + a1.pow(2) + a2.pow(2) + a3.pow(2)) as f64).sqrt()

}

pub fn compare(first: &[u8], second: &[u8]) -> u32
{
    let mut delta: f64 = 0.0;
    for x in 0..400 {
        for y in 0..400 {
            let ptr = (y * 400 + x) * 4;
            let col1: Rgb = [
                first[ptr + 0],
                first[ptr + 1],
                first[ptr + 2],
                first[ptr + 3],
            ];
            let col2: Rgb = [
                second[ptr + 0],
                second[ptr + 1],
                second[ptr + 2],
                second[ptr + 3],
            ];
            delta += compare_rgb(col1, col2);
        }
    }

    (delta * 0.005).round() as u32

}

pub fn pngread(file_name: &str) -> Vec<u8>
{
    let decoder = png::Decoder::new(File::open(file_name).unwrap());

    let mut reader = decoder.read_info().unwrap();

    let info = reader.info();

    assert_eq!(info.width, 400);
    assert_eq!(info.height, 400);

    let mut buf = vec![0; reader.output_buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    buf
}

pub fn gen_white() -> Vec<u8>
{
    let mut v = Vec::new();
    v.resize(640000, 255);
    v

}


#[test]
fn compare_against_white () {
    let ia = pngread("./test.png");
    let ib = gen_white();
    assert_eq!(compare(&ia, &ib), 135112);


}
