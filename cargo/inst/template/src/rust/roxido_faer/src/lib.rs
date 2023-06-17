use faer_core::MatRef;
use roxido::rbindings::*;
use roxido::Rval;

pub trait RvalFaer {
    fn as_matrix_f64(self) -> Result<MatRef<'static, f64>, &'static str>;
}

impl RvalFaer for Rval {
    fn as_matrix_f64(self) -> Result<MatRef<'static, f64>, &'static str> {
        Ok(unsafe {
            if Rf_isMatrix(self.0) == 0 {
                return Err("Not a matrix");
            }
            if Rf_isReal(self.0) == 0 {
                return Err("Not f64");
            }
            let nrow = Rf_nrows(self.0).try_into().unwrap();
            let ncol = Rf_ncols(self.0).try_into().unwrap();
            let slice = self.slice_double()?;
            MatRef::from_raw_parts(slice.as_ptr(), nrow, ncol, 1, nrow.try_into().unwrap())
        })
    }
}
