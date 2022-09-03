
use image::{Rgba, ImageBuffer};

use super::*;

#[derive(Clone)]
pub struct Painting {
    blocks: Vec<Block>,
    image: ImageBuffer<Rgba<u8>, Vec<u8>>,
}

pub enum PaintError {
    InvalidBlock,
    BlockError (BlockError),
}

impl From<BlockError> for PaintError {
    fn from(block_err: BlockError) -> PaintError {
        PaintError::BlockError(block_err)
    }
}

impl Painting {
    pub fn new(size: (u32, u32)) -> Self {
        Self {
            blocks: vec![Block { id: BlockId([0].to_vec()), pos: (0, 0), size: size}],
            image: ImageBuffer::from_fn(size.0, size.1, |_x, _y| Rgba::<u8>([255, 255, 255, 255])),
        }
    }

    pub fn apply_cmd(&mut self, cmd: ProgCmd) -> Result<(), PaintError>{
        use parser::ProgCmd::*;
        
        match cmd {
            Comment(_) => (),
            LineCut(id, dir, at) => {
                let idx = self.get_block_idx(id).ok_or(PaintError::InvalidBlock)?;
                let block = self.blocks.swap_remove(idx);
                let [b1, b2] = block.cut_line(dir, at)?;
                self.blocks.push(b1);
                self.blocks.push(b2);
            }
            _ => unimplemented!()
        }

        Ok(())
    }

    fn get_block_idx(&self, id: BlockId) -> Option<usize> {
        for i in 0..self.blocks.len() {
            if self.blocks[i].id == id {
                return Some(i);
            }
        }

        None
    }
}
