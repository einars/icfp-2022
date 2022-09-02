
pub mod unparse;
pub mod parse;


#[derive(Debug, Clone, PartialEq)]
pub struct BlockId(pub Vec<u32>);

#[derive(Debug, Clone, PartialEq)]
pub enum CutDirection {
    X,
    Y
}

#[derive(Debug, Clone, PartialEq)]
pub struct Color(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub enum ProgCmd {
    Comment(String),
    PointCut(BlockId, (u32, u32)),
    LineCut(BlockId, CutDirection, u32),
    Color(BlockId, Color),
    Swap(BlockId, BlockId),
    Merge(BlockId, BlockId),
}

/// Move between linear command and parsed text representations
///
/// ```
/// use parser::*;
///
/// fn b2(a:u32, b:u32) -> BlockId {
///     BlockId(vec![a, b])
/// }
/// 
/// let color = Color(0x04030201);
/// 
/// let p_tree = vec![
///     ProgCmd::Comment("Com".to_string()),
///     ProgCmd::PointCut(BlockId(vec![1, 0, 2]), (1, 2)),
///     ProgCmd::LineCut(b2(1, 0), CutDirection::X, 9),
///     ProgCmd::LineCut(b2(1, 0), CutDirection::Y, 9),
///     ProgCmd::Color(b2(1, 0), color),
///     ProgCmd::Swap(b2(1, 0), b2(1, 0)),
///     ProgCmd::Merge(b2(1, 0), b2(1, 0)),
/// ];
/// let p_source = 
/// "# Com
/// cut [1.0.2] [1,2]
/// cut [1.0] [x] 9
/// cut [1.0] [y] 9
/// color [1.0] [1,2,3,4]
/// swap [1.0] [1.0]
/// merge [1.0] [1.0]";
/// 
/// assert_eq!(tree_to_source(&p_tree), p_source);
/// assert_eq!(source_to_tree(p_source), p_tree);
/// 
/// ```


pub fn source_to_tree(s: &str) -> Vec<ProgCmd> {
    parse::parse(s)
}

pub fn tree_to_source(v: &Vec<ProgCmd>) -> String {
    unparse::unparse(v)
}


