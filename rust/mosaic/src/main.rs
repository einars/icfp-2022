use std::env;
use std::fs::File;

// use compadre::*;
use blocks::painting::Painting;
use blocks::Block;
use compadre::Rgb;
use parser::*;

#[derive(Debug)]
enum DirDir {
    XLeft,
    XRight,
    YTop,
    YBottom,
}

#[derive(Debug)]
struct SlicingResult {
    direction: DirDir,
    color: Rgb,
    n: u32,
    colordiff: f64,
}

fn calc_avgcolor(b: &Block, image: &[u8]) -> Rgb {
    let mut sr: usize = 0;
    let mut sg: usize = 0;
    let mut sb: usize = 0;
    let mut sa: usize = 0;
    for x in 0..b.size.0 - 1 {
        for y in 0..b.size.1 - 1 {
            let ptr = (4 * ((399 - (b.pos.1 + y)) * 400 + (b.pos.0 + x))) as usize;
            sr += image[ptr + 0] as usize;
            sg += image[ptr + 1] as usize;
            sb += image[ptr + 2] as usize;
            sa += image[ptr + 3] as usize;
        }
    }

    let area = (b.size.0 * b.size.1) as usize;
    [
        (sr / area) as u8,
        (sg / area) as u8,
        (sb / area) as u8,
        (sa / area) as u8,
    ]
}

fn calc_colordiff(b: &Block, image: &[u8], color: Rgb) -> f64 {
    let mut delta: f64 = 0.0;

    for x in 0..b.size.0 - 1 {
        for y in 0..b.size.1 - 1 {
            let ptr = (4 * ((399 - (b.pos.1 + y)) * 400 + (b.pos.0 + x))) as usize;
            let image_col = [
                image[ptr + 0],
                image[ptr + 1],
                image[ptr + 2],
                image[ptr + 3],
            ];
            delta += compadre::compare_rgb(image_col, color);
        }
    }

    delta / ((b.size.0 * b.size.1) as f64)
}

fn find_best_slice(b: &Block, image: &[u8]) -> Option<SlicingResult> {
    let mut best: Option<SlicingResult> = None;

    let min_slice = 20;
    let leeway = 0.0;

    let bx = b.pos.0;
    let by = b.pos.1;
    let bw = b.size.0;
    let bh = b.size.1;

    if bw > min_slice {
        for x in min_slice..b.size.0 - 1 {
            let new_block = Block {
                id: BlockId(vec![]),
                pos: b.pos,
                size: (x, b.size.1),
            };
            let new_color = calc_avgcolor(&new_block, image);
            let colordiff = calc_colordiff(&new_block, image, new_color);

            if best.is_none() || best.as_ref().unwrap().colordiff + leeway > colordiff {
                best = Some(SlicingResult {
                    direction: DirDir::XLeft,
                    colordiff,
                    n: x + b.pos.0,
                    color: new_color,
                });
            } else {
                break;
            };
        }

        for x in min_slice..bw - 3 {
            let new_block = Block {
                id: BlockId(vec![]),
                pos: (bx + bw - x, by),
                size: (x, bh),
            };
            let new_color = calc_avgcolor(&new_block, image);
            let colordiff = calc_colordiff(&new_block, image, new_color);

            if best.is_none() || best.as_ref().unwrap().colordiff + leeway > colordiff {
                best = Some(SlicingResult {
                    direction: DirDir::XRight,
                    colordiff,
                    n: bx + bw - x,
                    color: new_color,
                });
            } else {
                break;
            };
        }
    }

    if bh > min_slice {
        for y in min_slice..b.size.1 - 1 {
            let new_block = Block {
                id: BlockId(Vec::new()),
                pos: b.pos,
                size: (b.size.0, y),
            };
            let new_color = calc_avgcolor(&new_block, image);
            let colordiff = calc_colordiff(&new_block, image, new_color);

            if best.is_none() || best.as_ref().unwrap().colordiff + leeway > colordiff {
                best = Some(SlicingResult {
                    direction: DirDir::YTop,
                    colordiff,
                    n: y + b.pos.1,
                    color: new_color,
                });
            } else {
                break;
            };
        }

        for y in min_slice..bh - 3 {
            let new_block = Block {
                id: BlockId(Vec::new()),
                pos: (bx, bh + by - y),
                size: (bw, y),
            };
            let new_color = calc_avgcolor(&new_block, image);
            let colordiff = calc_colordiff(&new_block, image, new_color);

            if best.is_none() || best.as_ref().unwrap().colordiff + leeway > colordiff {
                best = Some(SlicingResult {
                    direction: DirDir::YBottom,
                    colordiff,
                    n: by + bh - y,
                    color: new_color,
                });
            } else {
                break;
            };
        }
    }

    // eprintln!("{:?}", best);

    best
}

fn mosaic_magic(file_name: &str) {
    let decoder = png::Decoder::new(File::open(file_name).unwrap());

    let mut reader = decoder.read_info().unwrap();

    let info = reader.info();

    assert_eq!(info.width, 400);
    assert_eq!(info.height, 400);

    let mut buf = vec![0; reader.output_buffer_size()];

    let info = reader.next_frame(&mut buf).unwrap();
    let bytes = &buf[..info.buffer_size()];

    let mut pt = Painting::new((400, 400));

    let mut block_id = BlockId(vec![0]);
    let mut program: Vec<ProgCmd> = vec![];

    let mut last_color = [0, 0, 0, 0];

    loop {
        let block = pt.get_block(&block_id).unwrap();
        let best = find_best_slice(&block, bytes);
        if best.is_none() {
            program.push(ProgCmd::Color(block_id, Color(last_color)));
            break;
        }

        let best = best.unwrap();

        let mut bid_to_color = block_id.clone();
        let mut bid_to_process = block_id.clone();

        match best.direction {
            DirDir::XLeft => {
                let cmd = ProgCmd::LineCut(block_id.clone(), CutDirection::X, best.n);
                pt.apply_cmd(&cmd).unwrap();
                program.push(cmd);
                bid_to_color.0.push(0);
                bid_to_process.0.push(1);
            }
            DirDir::XRight => {
                let cmd = ProgCmd::LineCut(block_id.clone(), CutDirection::X, best.n);
                pt.apply_cmd(&cmd).unwrap();
                program.push(cmd);
                bid_to_color.0.push(1);
                bid_to_process.0.push(0);
            }
            DirDir::YTop => {
                let cmd = ProgCmd::LineCut(block_id.clone(), CutDirection::Y, best.n);
                pt.apply_cmd(&cmd).unwrap();
                program.push(cmd);
                bid_to_color.0.push(0);
                bid_to_process.0.push(1);
            }
            DirDir::YBottom => {
                let cmd = ProgCmd::LineCut(block_id.clone(), CutDirection::Y, best.n);
                pt.apply_cmd(&cmd).unwrap();
                program.push(cmd);
                bid_to_color.0.push(1);
                bid_to_process.0.push(0);
            }
        }

        program.push(ProgCmd::Color(bid_to_color, Color(best.color)));
        last_color = best.color;
        block_id = bid_to_process;
    }

    println!("{}", parser::tree_to_source(&program));

    // let s = 100;
    // let n_versions: u32 = 400 / s;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 {
        mosaic_magic(&args[1]);
    } else {
        eprintln!("Usage: ./mosaic <image.png>");
        mosaic_magic("./9.png");
    }
}
