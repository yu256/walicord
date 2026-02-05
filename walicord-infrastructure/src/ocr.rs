use ocrs::{ImageSource, OcrEngine, OcrEngineParams};
use rten::Model;
use walicord_application::{
    ReceiptOcr, ReceiptOcrError,
    receipt::{OcrText, ReceiptImage},
};

pub struct OcrsReceiptOcr {
    engine: OcrEngine,
}

#[derive(Debug)]
struct AnyhowWrapper<T>(T);
impl<T: std::fmt::Display> std::fmt::Display for AnyhowWrapper<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T: std::fmt::Debug + std::fmt::Display> std::error::Error for AnyhowWrapper<T> {}

impl OcrsReceiptOcr {
    pub fn new(
        detection_model_path: &str,
        recognition_model_path: &str,
    ) -> Result<Self, ReceiptOcrError> {
        let detection_model =
            Model::load_file(detection_model_path).map_err(|err| ReceiptOcrError::ModelLoad {
                path: detection_model_path.into(),
                source: Box::new(err),
            })?;
        let recognition_model =
            Model::load_file(recognition_model_path).map_err(|err| ReceiptOcrError::ModelLoad {
                path: recognition_model_path.into(),
                source: Box::new(err),
            })?;

        let params = OcrEngineParams {
            detection_model: Some(detection_model),
            recognition_model: Some(recognition_model),
            ..OcrEngineParams::default()
        };

        let engine = OcrEngine::new(params).map_err(|err| ReceiptOcrError::EngineInit {
            source: Box::new(AnyhowWrapper(err)),
        })?;

        Ok(Self { engine })
    }
}

impl ReceiptOcr for OcrsReceiptOcr {
    fn extract_text(&self, image: &ReceiptImage<'_>) -> Result<OcrText, ReceiptOcrError> {
        let decoded =
            image::load_from_memory(image.bytes).map_err(|err| ReceiptOcrError::ImageDecode {
                source: Box::new(err),
            })?;
        let rgb = decoded.into_rgb8();
        let (width, height) = rgb.dimensions();
        let source = ImageSource::from_bytes(rgb.as_raw(), (width, height)).map_err(|err| {
            ReceiptOcrError::ImageDecode {
                source: Box::new(err),
            }
        })?;
        let input = self
            .engine
            .prepare_input(source)
            .map_err(|err| ReceiptOcrError::OcrRun {
                source: Box::new(AnyhowWrapper(err)),
            })?;
        let text = self
            .engine
            .get_text(&input)
            .map_err(|err| ReceiptOcrError::OcrRun {
                source: Box::new(AnyhowWrapper(err)),
            })?;

        Ok(OcrText {
            text,
            confidence: None,
        })
    }
}
