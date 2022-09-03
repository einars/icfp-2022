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

use parser::*;
use blocks::painting::{Painting, PaintError};
use blocks::Block;

pub type Rgb = [u8; 4];

pub fn compare_rgb(a: Rgb, b: Rgb) -> f64 {
    let a0: i32 = (a[0] as i32) - (b[0] as i32);
    let a1: i32 = (a[1] as i32) - (b[1] as i32);
    let a2: i32 = (a[2] as i32) - (b[2] as i32);
    let a3: i32 = (a[3] as i32) - (b[3] as i32);

    ((a0.pow(2) + a1.pow(2) + a2.pow(2) + a3.pow(2)) as f64).sqrt()
}

/// Salīdzina divus attēlus
pub fn compare(first: &[u8], second: &[u8]) -> u32 {
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

/// Ielasa png attēlu un atdod baitu buferi
pub fn pngread(file_name: &str) -> Vec<u8> {
    let decoder = png::Decoder::new(File::open(file_name).unwrap());

    let mut reader = decoder.read_info().unwrap();

    let info = reader.info();

    assert_eq!(info.width, 400);
    assert_eq!(info.height, 400);

    let mut buf = vec![0; reader.output_buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    buf
}

/// Uzģenerē defaulto balto attēlu
pub fn gen_white() -> Vec<u8> {
    let mut v = Vec::new();
    v.resize(640000, 255);
    v
}

#[test]
fn compare_against_white() {
    let ia = pngread("./test.png");
    let ib = gen_white();
    assert_eq!(compare(&ia, &ib), 135112);
}

fn get_block_size(b: &Block) -> u32
{
    b.size.0 * b.size.1
}

pub fn calc_cmd_score(cmd: &ProgCmd, p: &Painting) -> Result<u32, PaintError> {
    let base_cost;
    let canvas_size = 400 * 400;
    let mut block_size = 0;
    match cmd {

        ProgCmd::LineCut(block_id, _, _) => {
            base_cost = 7;
            block_size = get_block_size(p.get_block(block_id)?);
        },

        ProgCmd::PointCut(block_id, _) => {
            base_cost = 10;
            block_size = get_block_size(p.get_block(block_id)?);
        },

        ProgCmd::Color(block_id, _) => {
            base_cost = 5;
            block_size = get_block_size(p.get_block(block_id)?);
        },

        ProgCmd::Swap(block_id_1, _block_id_2) => {
            base_cost = 3;
            block_size = get_block_size(p.get_block(block_id_1)?);
        },

        ProgCmd::Merge(block_id_1, block_id_2) => {
            base_cost = 1;
            let bs1 = get_block_size(p.get_block(block_id_1)?);
            let bs2 = get_block_size(p.get_block(block_id_2)?);
            block_size = if bs1 > bs2 { bs1 } else { bs2 };
        },

        ProgCmd::Comment(_) => {
            base_cost = 0
        },
    }

    let cost = base_cost * canvas_size / block_size;
    // eprintln!("{} * {} / {} = {}", base_cost, canvas_size, block_size, cost);
    Ok(cost)
}

#[test]
pub fn test_cmd_scores()
{
    let mut pt = Painting::new((400, 400));
    let initial_cut = ProgCmd::LineCut(BlockId(vec![0]), CutDirection::X, 200);
    pt.apply_cmd(&initial_cut).expect("Cut was unsuccessful");

    let blk_fst = BlockId(vec![0, 0]);
    let _blk_snd = BlockId(vec![0, 1]);

    // 200x400 -> 200x200
    assert_eq!(
        calc_cmd_score(&ProgCmd::LineCut(blk_fst, CutDirection::X, 200), &pt).unwrap(),
        (7 * 400 * 400 / (200 * 400))
    );

}
