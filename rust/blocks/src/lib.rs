#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub id: BlockId,
    pub pos: (u32, u32),
    pub size: (u32, u32),
    pub color: u32,
}

pub type BlockId = Vec<u32>;

#[derive(Debug, Clone, Copy)]
pub enum LineCutDirection {
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockError {
    CoordOutOfBounds,
}

impl Block {
    pub fn color(&mut self, color: u32) {
        self.color = color;
    }

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
    ///
    /// let block = Block {
    ///     id: [0].to_vec(),
    ///     pos: (100, 100),
    ///     size: (100, 100),
    ///     color: 0,
    /// };
    ///
    /// assert_eq!(
    ///     block.clone().cut_line(LineCutDirection::X, 50),
    ///     Err(BlockError::CoordOutOfBounds),
    /// );
    ///
    /// assert_eq!(
    ///     block.clone().cut_line(LineCutDirection::X, 120),
    ///     Ok([
    ///         Block{
    ///             id: [0, 0].to_vec(),
    ///             pos: (100, 100),
    ///             size: (20, 100),
    ///             color: 0,
    ///         },
    ///         Block {
    ///             id: [0, 1].to_vec(),
    ///             pos: (120, 100),
    ///             size: (80, 100),
    ///             color: 0,
    ///         }
    ///     ].to_vec())
    /// );
    /// ```
    pub fn cut_line(self, direction: LineCutDirection, at: u32) -> Result<Vec<Block>, BlockError> {
        match direction {
            LineCutDirection::X => check_bound_1(self.pos.0, self.size.0, at),
            LineCutDirection::Y => check_bound_1(self.pos.1, self.size.1, at),
        }?;

        Ok(vec![
            Block {
                id: self.sub_id(0),
                color: self.color,
                pos: self.pos,
                size: match direction {
                    LineCutDirection::X => (at - self.pos.0, self.size.1),
                    LineCutDirection::Y => (self.size.0, at - self.pos.1),
                },
            },
            Block {
                id: self.sub_id(1),
                color: self.color,
                pos: match direction {
                    LineCutDirection::X => (at, self.pos.1),
                    LineCutDirection::Y => (self.pos.0, at),
                },
                size: match direction {
                    LineCutDirection::X => (self.pos.0 + self.size.0 - at, self.size.1),
                    LineCutDirection::Y => (self.size.0, self.pos.1 + self.size.1 - at),
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
    ///     color: 0,
    /// };
    ///
    /// assert_eq!(
    ///     block.clone().cut_point((120, 140)),
    ///     Ok([
    ///         Block{
    ///             id: [0, 3].to_vec(),
    ///             pos: (100, 100),
    ///             size: (20, 40),
    ///             color: 0,
    ///         },
    ///         Block{
    ///             id: [0, 2].to_vec(),
    ///             pos: (120, 100),
    ///             size: (80, 40),
    ///             color: 0,
    ///         },
    ///         Block{
    ///             id: [0, 1].to_vec(),
    ///             pos: (120, 140),
    ///             size: (80, 60),
    ///             color: 0,
    ///         },
    ///         Block{
    ///             id: [0, 0].to_vec(),
    ///             pos: (100, 140),
    ///             size: (20, 60),
    ///             color: 0,
    ///         },
    ///     ].to_vec())
    /// );
    /// ```
    pub fn cut_point(self, at: (u32, u32)) -> Result<Vec<Block>, BlockError> {
        check_bound_1(self.pos.0, self.size.0, at.0)?;
        check_bound_1(self.pos.1, self.size.1, at.1)?;

        Ok(vec![
            Block {
                id: self.sub_id(3),
                color: self.color,
                pos: self.pos,
                size: (at.0 - self.pos.0, at.1 - self.pos.1),
            },
            Block {
                id: self.sub_id(2),
                color: self.color,
                pos: (at.0, self.pos.1),
                size: (self.pos.0 + self.size.0 - at.0, at.1 - self.pos.1),
            },
            Block {
                id: self.sub_id(1),
                color: self.color,
                pos: (at.0, at.1),
                size: (self.pos.0 + self.size.0 - at.0, self.pos.1 + self.size.1 - at.1),
            },
            Block {
                id: self.sub_id(0),
                color: self.color,
                pos: (self.pos.0, at.1),
                size: (at.0 - self.pos.0, self.pos.1 + self.size.1 - at.1),
            },
        ])
    }

    fn sub_id(&self, id: u32) -> BlockId {
        let mut new_id = self.id.clone();
        new_id.push(id);
        new_id
    }
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
        assert_eq!(check_bound_1(200, 100, 50), Err(BlockError::CoordOutOfBounds));
        assert_eq!(check_bound_1(200, 100, 200), Err(BlockError::CoordOutOfBounds));
        assert_eq!(check_bound_1(200, 100, 201), Ok(()));
        assert_eq!(check_bound_1(200, 100, 250), Ok(()));
        assert_eq!(check_bound_1(200, 100, 299), Ok(()));
        assert_eq!(check_bound_1(200, 100, 300), Err(BlockError::CoordOutOfBounds));
        assert_eq!(check_bound_1(200, 100, 350), Err(BlockError::CoordOutOfBounds));
    }
}
