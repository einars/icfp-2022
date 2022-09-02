use std::convert::From;

pub mod unparse;

#[derive(Debug, Clone)]
pub struct BlockId(Vec<u32>);

#[derive(Debug, Clone)]
pub struct Point {
    x: u32,
    y: u32
}

#[derive(Debug, Clone)]
pub enum Orientation {
    Vertical,
    Horizontal
}

#[derive(Debug, Clone)]
pub struct Color(u32);

#[derive(Debug, Clone)]
pub enum ProgCmd {
    Comment(String),
    PointCut(BlockId, Point),
    LineCut(BlockId, Orientation, u32),
    Color(BlockId, Color),
    Swap(BlockId, BlockId),
    Merge(BlockId, BlockId),
}


#[derive(Debug)]
pub struct PainterImpl {
    lines: Vec<ProgCmd>
}

pub type PainterRepr = String;


#[allow(dead_code)]
fn b2(a:u32, b:u32) -> BlockId {
    BlockId(vec![a, b])
}

#[test]
fn test_sample () {
    let point = Point{ x: 1, y: 2 };

    let color = Color(0x04030201);

    let p2 = PainterImpl{ lines: vec![
        ProgCmd::Comment("Com".to_string()),
        ProgCmd::PointCut(b2(1, 0), point),
        ProgCmd::LineCut(b2(1, 0), Orientation::Horizontal, 9),
        ProgCmd::LineCut(b2(1, 0), Orientation::Vertical, 9),
        ProgCmd::Color(b2(1, 0), color),
        ProgCmd::Swap(b2(1, 0), b2(1, 0)),
        ProgCmd::Merge(b2(1, 0), b2(1, 0)),
    ]};
    let p2_expected = 
"# Com
cut 1.0 [1,2]
cut 1.0 [x] 9
cut 1.0 [y] 9
color 1.0 [1,2,3,4]
swap 1.0 1.0
merge 1.0 1.0";

    let p2_actual: PainterRepr = p2.into();

    assert_eq!(p2_actual, p2_expected);
}
