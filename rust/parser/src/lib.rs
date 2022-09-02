use std::convert::From;

pub mod unparse;
pub mod parse;

#[derive(Debug, Clone, PartialEq)]
pub struct BlockId(pub Vec<u32>);

#[derive(Debug, Clone, PartialEq)]
pub struct Point {
    pub x: u32,
    pub y: u32
}

#[derive(Debug, Clone, PartialEq)]
pub enum Orientation {
    Vertical,
    Horizontal
}

#[derive(Debug, Clone, PartialEq)]
pub struct Color(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum ProgCmd {
    Comment(String),
    PointCut(BlockId, Point),
    LineCut(BlockId, Orientation, u32),
    Color(BlockId, Color),
    Swap(BlockId, BlockId),
    Merge(BlockId, BlockId),
}


#[derive(Debug, PartialEq)]
pub struct PainterImpl(pub Vec<ProgCmd>);

#[derive(Debug, PartialEq)]
pub struct PainterRepr(pub String);


#[allow(dead_code)]
fn b2(a:u32, b:u32) -> BlockId {
    BlockId(vec![a, b])
}

#[test]
fn test_sample () {
    let point = Point{ x: 1, y: 2 };

    let color = Color(0x04030201);

    let p2_tree = PainterImpl(vec![
        ProgCmd::Comment("Com".to_string()),
        ProgCmd::PointCut(BlockId(vec![1, 0, 2]), point),
        ProgCmd::LineCut(b2(1, 0), Orientation::Horizontal, 9),
        ProgCmd::LineCut(b2(1, 0), Orientation::Vertical, 9),
        ProgCmd::Color(b2(1, 0), color),
        ProgCmd::Swap(b2(1, 0), b2(1, 0)),
        ProgCmd::Merge(b2(1, 0), b2(1, 0)),
    ]);
    let p2_source = 
"# Com
cut 1.0.2 [1,2]
cut 1.0 [x] 9
cut 1.0 [y] 9
color 1.0 [1,2,3,4]
swap 1.0 1.0
merge 1.0 1.0";

    let p2_tree_to_source: PainterRepr = (&p2_tree).into();

    assert_eq!(p2_tree_to_source.0, p2_source);

    let p2_source_to_tree: PainterImpl = (p2_source).into();

    assert_eq!(p2_source_to_tree, p2_tree);
}
