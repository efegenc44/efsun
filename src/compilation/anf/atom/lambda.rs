use crate::{compilation::anf, metadata::CaptureMetadataId, resolution::renamer::UniqueName};

pub struct Lambda {
    variable: UniqueName,
    expression: Box<anf::Expression>,
    anf_capture_id: CaptureMetadataId,
}

impl Lambda {
    pub fn new(
        variable: UniqueName,
        expression: anf::Expression,
        anf_capture_id: CaptureMetadataId,
    ) -> Self {
        Self {
            variable,
            expression: Box::new(expression),
            anf_capture_id,
        }
    }

    pub fn variable(&self) -> UniqueName {
        self.variable
    }

    pub fn expression(&self) -> &anf::Expression {
        &self.expression
    }

    pub fn anf_capture_id(&self) -> CaptureMetadataId {
        self.anf_capture_id
    }
}
