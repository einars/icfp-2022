//! Use the `Block` struct to create and manipulate blocks.
//! Use the `Painting` struct to hold and update an entire painting.

use parser::*;

pub mod painting;
pub use painting::{Painting, PaintError};

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub id: BlockId,
    pub pos: (u32, u32),
    pub size: (u32, u32),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockError {
    CoordOutOfBounds,
    NotAdjacent,
}

impl Block {
    /// Cut by line - consumes the block and returns two new blocks in its stead.
    /// # Arguments
    ///
    /// * `at` - note that `at` is the *abslute* position (relative to the canvas
    /// and not to the block) at which to cut. See the examples below.
    ///
    /// # Examples
    ///
    /// ```
    /// use blocks::*;
    /// use parser::*; // CutDirection
    ///
    /// let block = Block {
    ///     id: [0].to_vec(),
    ///     pos: (100, 100),
    ///     size: (100, 100),
    /// };
    ///
    /// assert_eq!(
    ///     block.clone().cut_line(CutDirection::X, 50),
    ///     Err(BlockError::CoordOutOfBounds),
    /// );
    ///
    /// assert_eq!(
    ///     block.clone().cut_line(CutDirection::X, 120),
    ///     Ok([
    ///         Block{
    ///             id: [0, 0].to_vec(),
    ///             pos: (100, 100),
    ///             size: (20, 100),
    ///         },
    ///         Block {
    ///             id: [0, 1].to_vec(),
    ///             pos: (120, 100),
    ///             size: (80, 100),
    ///         }
    ///     ].to_vec())
    /// );
    /// 
    /// assert_eq!(
    ///     block.clone().cut_line(CutDirection::Y, 120),
    ///     Ok([
    ///         Block{
    ///             id: [0, 0].to_vec(),
    ///             pos: (100, 100),
    ///             size: (100, 20),
    ///         },
    ///         Block {
    ///             id: [0, 1].to_vec(),
    ///             pos: (100, 120),
    ///             size: (100, 80),
    ///         }
    ///     ].to_vec())
    /// );    
    /// ```
    pub fn cut_line(self, direction: CutDirection, at: u32) -> Result<[Block; 2], BlockError> {
        match direction {
            CutDirection::X => check_bound_1(self.pos.0, self.size.0, at),
            CutDirection::Y => check_bound_1(self.pos.1, self.size.1, at),
        }?;

        Ok([
            Block {
                id: self.sub_id(0),
                pos: self.pos,
                size: match direction {
                    CutDirection::X => (at - self.pos.0, self.size.1),
                    CutDirection::Y => (self.size.0, at - self.pos.1),
                },
            },
            Block {
                id: self.sub_id(1),
                pos: match direction {
                    CutDirection::X => (at, self.pos.1),
                    CutDirection::Y => (self.pos.0, at),
                },
                size: match direction {
                    CutDirection::X => (self.pos.0 + self.size.0 - at, self.size.1),
                    CutDirection::Y => (self.size.0, self.pos.1 + self.size.1 - at),
                },
            },
        ])
    }

    /// Cut by point - consumes the block and returns four new blocks in its stead.
    /// # Arguments
    ///
    /// * `at` - note that `at` is the *abslute* position (relative to the canvas
    /// and not to the block) at which to cut. See the examples below.
    ///
    /// # Examples
    ///
    /// ```
    /// use blocks::*;
    ///
    /// let block = Block {
    ///     id: [0].to_vec(),
    ///     pos: (100, 100),
    ///     size: (100, 100),
    /// };
    ///
    /// assert_eq!(
    ///     block.clone().cut_point((120, 140)),
    ///     Ok([
    ///         Block{
    ///             id: [0, 0].to_vec(),
    ///             pos: (100, 100),
    ///             size: (20, 40),
    ///         },
    ///         Block{
    ///             id: [0, 1].to_vec(),
    ///             pos: (120, 100),
    ///             size: (80, 40),
    ///         },
    ///         Block{
    ///             id: [0, 2].to_vec(),
    ///             pos: (120, 140),
    ///             size: (80, 60),
    ///         },
    ///         Block{
    ///             id: [0, 3].to_vec(),
    ///             pos: (100, 140),
    ///             size: (20, 60),
    ///         },
    ///     ].to_vec())
    /// );
    /// ```
    pub fn cut_point(self, at: (u32, u32)) -> Result<[Block; 4], BlockError> {
        check_bound_1(self.pos.0, self.size.0, at.0)?;
        check_bound_1(self.pos.1, self.size.1, at.1)?;

        Ok([
            Block {
                id: self.sub_id(0),
                pos: self.pos,
                size: (at.0 - self.pos.0, at.1 - self.pos.1),
            },
            Block {
                id: self.sub_id(1),
                pos: (at.0, self.pos.1),
                size: (self.pos.0 + self.size.0 - at.0, at.1 - self.pos.1),
            },
            Block {
                id: self.sub_id(2),
                pos: (at.0, at.1),
                size: (
                    self.pos.0 + self.size.0 - at.0,
                    self.pos.1 + self.size.1 - at.1,
                ),
            },
            Block {
                id: self.sub_id(3),
                pos: (self.pos.0, at.1),
                size: (at.0 - self.pos.0, self.pos.1 + self.size.1 - at.1),
            },
        ])
    }

    pub fn swap(&mut self, other: &mut Block) {
        std::mem::swap(&mut self.id, &mut other.id);
    }

    /// Merges a block into another - requires you to specify a new id for the block
    /// 
    /// # Examples
    /// ```
    /// use blocks::*;
    /// 
    /// let block = Block { id: [0].to_vec(), pos: (100, 100), size: (40, 60)};
    /// let block_n = Block { id: [0].to_vec(), pos: (100, 160), size: (40, 30)};
    /// let block_w = Block { id: [0].to_vec(), pos: (50, 100), size: (50, 60)};
    /// 
    /// let mut block2 = block.clone();
    /// block2.merge(block_n, 1);
    /// assert_eq!(block2.pos, (100, 100));
    /// assert_eq!(block2.size, (40, 90));
    /// 
    /// let mut block3 = block.clone();
    /// block3.merge(block_w, 1);
    /// assert_eq!(block3.pos, (50, 100));
    /// assert_eq!(block3.size, (90, 60));
    /// ```
    pub fn merge(&mut self, other: Block, new_id: u32) -> Result<(), BlockError> {
        self.is_adjacent(&other)?;

        self.id = BlockId(vec![new_id]);
        if self.pos.0 == other.pos.0 { // matching x-es
            self.pos = (self.pos.0, std::cmp::min(self.pos.1, other.pos.1));
            self.size = (self.size.0, self.size.1 + other.size.1);
        } else { // matching y-s
            self.pos = (std::cmp::min(self.pos.0, other.pos.0), self.pos.1);
            self.size = (self.size.0 + other.size.0, self.size.1);
        }

        Ok(())
    }

    pub fn sub_id(&self, id: u32) -> BlockId {
        let mut wrapped_id = self.id.clone();
        let BlockId(ref mut new_id) = wrapped_id;
        new_id.push(id);
        wrapped_id
    }

    pub fn is_adjacent(&self, other: &Block) -> Result<(), BlockError> {
        if !is_adjacent_low_level_x(self, other)
            && !is_adjacent_low_level_y(self, other)
            && !is_adjacent_low_level_x(other, self)
            && !is_adjacent_low_level_y(other, self)
        {
            return Err(BlockError::NotAdjacent);
        }

        Ok(())
    }
}

fn is_adjacent_low_level_x(x: &Block, y: &Block) -> bool {
    x.pos.0 + x.size.0 == y.pos.0 && x.pos.1 == y.pos.1 && x.size.1 == y.size.1
}

fn is_adjacent_low_level_y(x: &Block, y: &Block) -> bool {
    x.pos.1 + x.size.1 == y.pos.1 && x.pos.0 == y.pos.0 && x.size.0 == y.size.0
}

fn check_bound_1(pos: u32, size: u32, val: u32) -> Result<(), BlockError> {
    if (val <= pos) || (val >= pos + size) {
        Err(BlockError::CoordOutOfBounds)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_bounds() {
        assert_eq!(
            check_bound_1(200, 100, 50),
            Err(BlockError::CoordOutOfBounds)
        );
        assert_eq!(
            check_bound_1(200, 100, 200),
            Err(BlockError::CoordOutOfBounds)
        );
        assert_eq!(check_bound_1(200, 100, 201), Ok(()));
        assert_eq!(check_bound_1(200, 100, 250), Ok(()));
        assert_eq!(check_bound_1(200, 100, 299), Ok(()));
        assert_eq!(
            check_bound_1(200, 100, 300),
            Err(BlockError::CoordOutOfBounds)
        );
        assert_eq!(
            check_bound_1(200, 100, 350),
            Err(BlockError::CoordOutOfBounds)
        );
    }

    #[test]
    fn test_swap() {
        let mut block1 = Block {
            id: BlockId([0].to_vec()),
            pos: (0, 0),
            size: (0, 0),
        };
        let mut block2 = Block {
            id: BlockId([1].to_vec()),
            pos: (0, 0),
            size: (0, 0),
        };

        block1.swap(&mut block2);
        assert_eq!(block1.id, BlockId([1].to_vec()));
        assert_eq!(block2.id, BlockId([0].to_vec()));
    }

    #[test]
    fn test_is_adjacent() {
        let block = Block { id: BlockId([0].to_vec()), pos: (100, 100), size: (40, 60)};
        let block_n = Block { id: BlockId([0].to_vec()), pos: (100, 160), size: (40, 30)};
        let block_e = Block { id: BlockId([0].to_vec()), pos: (140, 100), size: (20, 60)};
        let block_s = Block { id: BlockId([0].to_vec()), pos: (100, 80), size: (40, 20)};
        let block_w = Block { id: BlockId([0].to_vec()), pos: (50, 100), size: (50, 60)};

        assert_eq!(block.is_adjacent(&block_n), Ok(()));
        assert_eq!(block.is_adjacent(&block_e), Ok(()));
        assert_eq!(block.is_adjacent(&block_s), Ok(()));
        assert_eq!(block.is_adjacent(&block_w), Ok(()));

        assert_eq!(block_s.is_adjacent(&block_n), Err(BlockError::NotAdjacent));
        assert_eq!(block_s.is_adjacent(&block_e), Err(BlockError::NotAdjacent));
    }
}
