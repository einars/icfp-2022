use std::fmt;
use crate::*;

pub fn unparse(v: &Vec<ProgCmd>) -> String {
    let x: Vec<String> = v.iter().map(|l| {
        format!("{}", l)
    }).collect();
    x.join("\n")
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let vs: Vec<String> = self.0.iter().map(|n| format!("{}", n)).collect();
        write!(f, "[{}]", vs.join("."))
    }
}

impl fmt::Display for CutDirection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CutDirection::X => write!(f, "[x]"),
            CutDirection::Y => write!(f, "[y]"),
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let c = self.0;
        write!(f, "[{},{},{},{}]", c[0], c[1], c[2], c[3])
    }
}


impl fmt::Display for ProgCmd {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        match self {
            ProgCmd::Comment(s)       => write!(f, "# {}", s),
            ProgCmd::PointCut(b, p)   => write!(f, "cut {} [{},{}]", b, p.0, p.1),
            ProgCmd::LineCut(b, o, n) => write!(f, "cut {} {} {}", b, o, n),
            ProgCmd::Color(b, c)      => write!(f, "color {} {}", b, c),
            ProgCmd::Swap(ba, bb)     => write!(f, "swap {} {}", ba, bb),
            ProgCmd::Merge(ba, bb)    => write!(f, "merge {} {}", ba, bb),
        }


    }
}
