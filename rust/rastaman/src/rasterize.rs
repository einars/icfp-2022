use crate::Classifier;

use super::ImgBuf;
use smartcore::linalg::naive::dense_matrix::*;
use smartcore::tree::decision_tree_classifier::*;

pub trait Rasterizer {
    type Error;

    fn rasterize_into(&self, img: &ImgBuf, sink: &mut ImgBuf) -> Result<(), Self::Error>;

    fn rasterize(&self, img: &ImgBuf) -> Result<ImgBuf, Self::Error> {
        let mut new = image::ImageBuffer::new(img.width(), img.height());
        self.rasterize_into(img, &mut new)?;

        Ok(new)
    }
}

#[derive(Debug)]
pub enum RasterizerError {
    ClassifierFailed,
    DecisionTreeError(String),
}

impl From<smartcore::error::Failed> for RasterizerError {
    fn from(err: smartcore::error::Failed) -> Self {
        RasterizerError::DecisionTreeError(format!("{err:?}"))
    }
}

/// Rasterize an image using decision trees from `smartcore` and
/// any provided color classifier.
pub struct DecisionTreeRasterizer<C: Classifier> {
    classifier: C,
    img_matrix: DenseMatrix<f32>,
    classified_vec: Vec<f32>,
    params: DecisionTreeClassifierParameters,
    center_diameter: u32,
}

impl<C: Classifier> DecisionTreeRasterizer<C> {
    pub fn from_image(img: &ImgBuf, classifier: C) -> Result<Self, RasterizerError> {
        let cvec = classifier
            .classify(img)
            .ok_or(RasterizerError::ClassifierFailed)?
            .into_iter()
            .map(|x| x as f32)
            .collect();

        Ok(Self {
            classifier,
            classified_vec: cvec,
            img_matrix: {
                let pixel_count = (img.width() * img.height()) as usize;
                let mut data = Vec::with_capacity(2 * pixel_count);
                for x in 0..img.width() {
                    for _y in 0..img.height() {
                        data.push(x as f32);
                    }
                }
                for _x in 0..img.width() {
                    for y in 0..img.height() {
                        data.push(y as f32);
                    }
                }
                DenseMatrix::new(pixel_count, 2, data)
            },
            params: DecisionTreeClassifierParameters {
                max_depth: Some(5),
                ..Default::default()
            },
            center_diameter: 1,
        })
    }

    pub fn with_depth(mut self, depth: u16) -> Self {
        self.params.max_depth = Some(depth);
        self
    }

    pub fn with_criterion(mut self, criterion: SplitCriterion) -> Self {
        self.params.criterion = criterion;
        self
    }

    pub fn with_diameter(mut self, diameter: u32) -> Self {
        self.center_diameter = diameter;
        self
    }
}

impl<C: Classifier> Rasterizer for DecisionTreeRasterizer<C> {
    type Error = RasterizerError;

    fn rasterize_into(&self, img: &ImgBuf, sink: &mut ImgBuf) -> Result<(), Self::Error> {
        let tree = DecisionTreeClassifier::fit(
            &self.img_matrix,
            &self.classified_vec,
            self.params.clone(),
        )?;
        let fit = tree.predict(&self.img_matrix)?;
        let colormap = self.classifier.colormap();

        for x in 0..img.width() {
            for y in 0..img.height() {
                let cd = self.center_diameter;
                let cx = (x / cd) * cd + (cd / 2);
                let cy = (y / cd) * cd + (cd / 2);
                let color_id = fit[(cx * img.height() + cy) as usize] as u32;
                sink.put_pixel(
                    x,
                    y,
                    colormap
                        .get(&color_id)
                        .ok_or_else(|| {
                            panic!("raw id was {:?}", fit[(x * img.height() + y) as usize])
                        })
                        .unwrap()
                        .clone(),
                );
            }
        }
        Ok(())
    }
}
