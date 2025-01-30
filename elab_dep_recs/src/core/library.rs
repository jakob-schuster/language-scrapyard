use std::path::Path;
use std::rc::Rc;

use crate::util::{self, Location, RecField};

use super::{EvalError, Vtm};

pub fn foreign(
    location: &Location,
    index: usize,
) -> Result<Rc<dyn Fn(&Location, &[Vtm]) -> Result<Vtm, EvalError>>, EvalError> {
    match index {
        0 => Ok(Rc::new(csv_ty)),
        1 => Ok(Rc::new(csv_first)),
        _ => Err(EvalError::new(location, "foreign function does not exist")),
    }
}

pub fn csv_ty(location: &Location, vtms: &[Vtm]) -> Result<Vtm, EvalError> {
    match vtms {
        [Vtm::Str { s }] => {
            // open the file
            let mut rdr = csv::ReaderBuilder::new()
                .from_path(Path::new(&s))
                .map_err(|_| EvalError::new(location, "could not load file"))?;

            // get the type from the headers
            Ok(Vtm::RecTy {
                fields: rdr
                    .headers()
                    .map_err(|_| EvalError::new(location, "CSV file had no headers"))?
                    .iter()
                    .map(|header| RecField::new(header.to_string(), Vtm::StrTy))
                    .collect(),
            })
        }
        _ => Err(EvalError::new(
            &location,
            "bad arguments given to function?!",
        )),
    }
}

pub fn csv_first(location: &Location, vtms: &[Vtm]) -> Result<Vtm, EvalError> {
    match vtms {
        [Vtm::Str { s }] => {
            // open the file
            let mut rdr = csv::ReaderBuilder::new()
                .from_path(Path::new(&s))
                .map_err(|_| EvalError::new(location, "could not load file"))?;

            // get the type from the headers
            Ok(Vtm::Rec {
                fields: rdr
                    .headers()
                    .map_err(|_| EvalError::new(location, "CSV file had no headers"))?
                    .iter()
                    .map(|header| {
                        RecField::new(
                            header.to_string(),
                            Vtm::Str {
                                s: "test".to_string(),
                            },
                        )
                    })
                    .collect(),
            })
        }
        _ => Err(EvalError::new(
            &location,
            "bad arguments given to function?!",
        )),
    }
}
