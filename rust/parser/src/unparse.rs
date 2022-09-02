
use std::fmt;
use crate::*;

impl From<PainterImpl> for PainterRepr {
    // unparse
    fn from (v: PainterImpl) -> Self {
        let x: Vec<String> = v.lines.into_iter().map(|l| {
            format!("{}", l)
        }).collect();
        x.join("\n")
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vs: Vec<String> = self.0.iter().map(|n| format!("{}", n)).collect();
        write!(f, "{}", vs.join("."))
    }
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{},{}]", self.x, self.y)
    }
}

impl fmt::Display for Orientation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Orientation::Horizontal => write!(f, "[x]"),
            Orientation::Vertical => write!(f, "[y]"),
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bs = self.0.to_le_bytes();
        write!(f, "[{},{},{},{}]", bs[0], bs[1], bs[2], bs[3])
    }
}


impl fmt::Display for ProgCmd {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        match self {
            ProgCmd::Comment(s)       => write!(f, "# {}", s),
            ProgCmd::PointCut(b, p)   => write!(f, "cut {} {}", b, p),
            ProgCmd::LineCut(b, o, n) => write!(f, "cut {} {} {}", b, o, n),
            ProgCmd::Color(b, c)      => write!(f, "color {} {}", b, c),
            ProgCmd::Swap(ba, bb)     => write!(f, "swap {} {}", ba, bb),
            ProgCmd::Merge(ba, bb)    => write!(f, "merge {} {}", ba, bb),
        }


    }
}