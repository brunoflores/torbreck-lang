#[derive(Debug, Copy, Clone)]
pub struct Header {
    wosize: usize,
    color: Color,
    tag: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Color {
    White,
}

impl Header {
    pub fn new() -> Self {
        Header {
            wosize: 0,
            tag: 0,
            color: Color::White,
        }
    }

    pub fn newtag(t: usize) -> Self {
        Header {
            wosize: 0,
            tag: t,
            color: Color::White,
        }
    }
}
