
use image::{Rgba, ImageBuffer};

use super::*;

type ImgBuf = ImageBuffer<Rgba<u8>, Vec<u8>>;

#[derive(Debug, Clone)]
pub struct Painting<'a> {
    pub blocks: Vec<Block>,
    pub size: (u32, u32),
    pub image: std::borrow::Cow<'a, ImgBuf>,
    last_id: u32,
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

impl<'a> Painting<'a> {
    pub fn new(size: (u32, u32)) -> Self {
        Self {
            blocks: vec![Block { id: BlockId([0].to_vec()), pos: (0, 0), size: size}],
            size: size,
            image: std::borrow::Cow::<'a, _>::Owned(ImageBuffer::from_fn(size.0, size.1, |_x, _y| Rgba::<u8>([255, 255, 255, 255]))),
            last_id: 0,
        }
    }

    pub fn cow_clone(prev: &'a Self) -> Self {
        Self {
            blocks: prev.blocks.clone(),
            size: prev.size,
            image: prev.image.clone(),
            last_id: 0,
        }
    }

    pub fn from_program(size: (u32, u32), program: &Vec<ProgCmd>) -> Result<Self, PaintError> {
        let mut painting = Self::new(size);
        for cmd in program.iter() {
            painting.apply_cmd(cmd)?;
        }
        Ok(painting)
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
            PointCut(id, at) => {
                let idx = self.get_block_idx(id)?;
                let block = self.blocks.swap_remove(idx);
                let [b1, b2, b3, b4] = block.cut_point(*at)?;
                self.blocks.push(b1);
                self.blocks.push(b2);
                self.blocks.push(b3);
                self.blocks.push(b4);
            },
            Color(id, color) => {
                let idx = self.get_block_idx(id)?;
                let block = &self.blocks[idx];
                let image = self.image.to_mut();
                for x in block.pos.0..(block.pos.0 + block.size.0) {
                    for y in block.pos.1..(block.pos.1 + block.size.1) {
                        image.put_pixel(x, self.size.1 - y - 1, Rgba(color.0));
                    }
                }
            },
            Merge(id1, id2) => {
                let block2 = self.blocks.swap_remove(self.get_block_idx(id2)?).clone();
                let idx = self.get_block_idx(id1)?;
                let block1 = &mut self.blocks[idx];
                self.last_id += 1;
                block1.merge(block2, self.last_id)?;
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
