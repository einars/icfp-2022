use blocks::Painting;
use image::Rgba;
use parser::{CutDirection, ProgCmd};

pub type ImgBuf = image::ImageBuffer<image::Rgba<u8>, Vec<u8>>;

pub trait Plotter {
    fn generate(&self, img: &ImgBuf) -> Vec<ProgCmd>;
}

pub struct EagerBeaver {}

impl Plotter for EagerBeaver {
    fn generate(&self, img: &ImgBuf) -> Vec<ProgCmd> {
        let mut painting = Painting::new((img.width(), img.height()));
        let mut res = Vec::new();

        let mut px: Option<u32> = None;
        let mut py: Option<u32> = None;
        for y in 0..img.width() {
            for x in 0..img.height() {
                let pixel = img.get_pixel(x, img.height() - y - 1);
                let ppix = painting.image.get_pixel(x, img.height() - y - 1);

                macro_rules! ready {
                    () => {
                        match (px, py) {
                            (Some(px), Some(py)) => {
                                (px as i32 - x as i32).abs() + (py as i32 - y as i32).abs() > 5
                                && (py == y || (py as i32 - y as i32).abs() > 5)
                            }
                            _ => true,
                        }
                    };
                }

                macro_rules! same_line {
                    () => {
                        if let Some(py) = py {
                            y == py
                        } else {
                            y == 0
                        }
                    };
                }

                macro_rules! done {
                    () => {
                        px = Some(x);
                        py = Some(y);
                    };
                }

                macro_rules! color {
                    () => {{
                        let id = painting.block_by_pos((x, y)).unwrap();
                        let cmd = ProgCmd::Color(id, to_color(pixel));
                        painting.apply_cmd(&cmd).unwrap();
                        res.push(cmd);
                    }};
                }

                macro_rules! vcut {
                    () => {
                        let id = painting.block_by_pos((x, y)).unwrap();
                        let cmd = ProgCmd::LineCut(id, CutDirection::X, x);
                        painting.apply_cmd(&cmd).unwrap();
                        res.push(cmd);
                    };
                }

                macro_rules! hcut {
                    () => {
                        let id = painting.block_by_pos((x, y)).unwrap();
                        let cmd = ProgCmd::LineCut(id, CutDirection::Y, y);
                        painting.apply_cmd(&cmd).unwrap();
                        res.push(cmd);
                    };
                }

                macro_rules! pcut {
                    () => {
                        let id = painting.block_by_pos((x, y)).unwrap();
                        let cmd = ProgCmd::PointCut(id, (x, y));
                        painting.apply_cmd(&cmd).unwrap();
                        res.push(cmd);
                    };
                }

                macro_rules! merge {
                    () => {
                        let id1 = painting.block_by_pos((x, y)).unwrap();
                        let id2 = painting.block_by_pos((x - 1, y)).unwrap();
                        let cmd = ProgCmd::Merge(id1, id2);
                        painting.apply_cmd(&cmd).unwrap();
                        res.push(cmd);
                    };
                }

                if ppix != pixel {
                    match (x, y) {
                        (0, 0) => {
                            color!();
                            done!();
                        }
                        (_, _) if ready!() && same_line!() => {
                            vcut!();
                            color!();
                            merge!();
                            done!();
                        }
                        (0, _) if ready!() => {
                            hcut!();
                            color!();
                            done!();
                        }
                        (_, _) if ready!() => {
                            pcut!();
                            color!();
                            merge!();
                            done!();
                        }
                        _ => (), // We're not yet ready to paint again
                    }
                }
            }
        }

        res
    }
}

fn to_color(color: &Rgba<u8>) -> parser::Color {
    let Rgba(col) = color;
    parser::Color(*col)
}

#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
