
use image::{Rgba, ImageBuffer};

use super::*;

#[derive(Debug, Clone)]
pub struct Painting {
    pub blocks: Vec<Block>,
    pub size: (u32, u32),
    pub image: ImageBuffer<Rgba<u8>, Vec<u8>>,
}

#[derive(Debug)]
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
            size: size,
            image: ImageBuffer::from_fn(size.0, size.1, |_x, _y| Rgba::<u8>([255, 255, 255, 255])),
        }
    }

    pub fn apply_cmd(&mut self, cmd: &ProgCmd) -> Result<(), PaintError>{
        use parser::ProgCmd::*;
        
        match cmd {
            Comment(_) => (),
            LineCut(id, dir, at) => {
                let idx = self.get_block_idx(id)?;
                let block = self.blocks.swap_remove(idx);
                let [b1, b2] = block.cut_line(dir.clone(), *at)?;
                self.blocks.push(b1);
                self.blocks.push(b2);
            },
            Color(id, color) => {
                let idx = self.get_block_idx(id)?;
                let block = &self.blocks[idx];
                for x in block.pos.0..(block.pos.0 + block.size.0) {
                    for y in block.pos.1..(block.pos.1 + block.size.1) {
                        self.image.put_pixel(x, self.size.1 - y - 1, Rgba(color.0));
                    }
                }
            }
            _ => unimplemented!()
        }

        Ok(())
    }

    pub fn block_by_pos(&self, pos: (u32, u32)) -> Option<BlockId> {
        for i in 0..self.blocks.len() {
            let block = &self.blocks[i];
            if block.pos.0 <= pos.0 && block.pos.0 + block.size.0 > pos.0 && block.pos.1 <= pos.1 && block.pos.1 + block.size.1 > pos.1 {
                return Some(block.id.clone());
            }
        }

        None
    }

    pub fn get_block(&self, id: &BlockId) -> Result<&Block, PaintError> {
        let idx = self.get_block_idx(id)?;
        Ok(&self.blocks[idx])
    }

    fn get_block_idx(&self, id: &BlockId) -> Result<usize, PaintError> {
        for i in 0..self.blocks.len() {
            if self.blocks[i].id == *id {
                return Ok(i);
            }
        }

        Err(PaintError::InvalidBlock)
    }
}
