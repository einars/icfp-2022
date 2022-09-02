impl From<&str> for PainterImpl {
    // parse
    fn from (v: &str) -> Self {
        let l : ProgCmd = ProgCmd::Comment(v.to_string());
        PainterImpl { lines: vec!(l) }
    }
}

impl From<&PainterRepr> for PainterImpl {
    // parse
    fn from (v: &PainterRepr) -> Self {
        v.as_str().into()
    }
}


