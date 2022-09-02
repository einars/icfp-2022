use std::fs::File;
use std::env;

fn avgcol(bytes: &[u8], bx: usize, by: usize, s: usize) -> [u8;4] {

    let mut sr:usize = 0;
    let mut sg:usize = 0;
    let mut sb:usize = 0;
    let mut sa:usize = 0;

    for x in 0..(s - 1) {
        for y in 0..(s - 1) {
            let ptr = 4 * ((399 - (by + y)) * 400 + (bx + x));
            sr += bytes[ptr + 0] as usize;
            sg += bytes[ptr + 1] as usize;
            sb += bytes[ptr + 2] as usize;
            sa += bytes[ptr + 3] as usize;
        }
    }
    let area: usize = s * s;
    [(sr / area) as u8, (sg / area) as u8, (sb / area) as u8, (sa / area) as u8]

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

    // let s = 100;
    // let n_versions: u32 = 400 / s;

    println!("cut [0] [200,200]");
    let pts = avgcol(bytes, 0, 0, 200);
    println!("color [0.0] [{},{},{},{}]", pts[0], pts[1], pts[2], pts[3]);
    let pts = avgcol(bytes, 200, 0, 200);
    println!("color [0.1] [{},{},{},{}]", pts[0], pts[1], pts[2], pts[3]);
    let pts = avgcol(bytes, 200, 200, 200);
    println!("color [0.2] [{},{},{},{}]", pts[0], pts[1], pts[2], pts[3]);
    let pts = avgcol(bytes, 0, 200, 200);
    println!("color [0.3] [{},{},{},{}]", pts[0], pts[1], pts[2], pts[3]);


}

fn main() {
    let args: Vec<String> = env::args().collect();


    if args.len() > 1 {
        mosaic_magic(&args[1]);
    } else {
        eprintln!("Usage: ./mosaic <image.png>")
    }

}
