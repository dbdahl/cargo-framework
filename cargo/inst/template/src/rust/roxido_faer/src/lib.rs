use faer_core::MatRef;
use roxido::{Pc, RMatrix};

pub trait RMatrix2Faer {
    fn as_faer_f64(self) -> Result<MatRef<'static, f64>, &'static str>;
}

impl RMatrix2Faer for RMatrix {
    fn as_faer_f64(self) -> Result<MatRef<'static, f64>, &'static str> {
        Ok(unsafe {
            use roxido::rbindings::*;
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

pub trait Faer2RMatrix {
    fn to_rmatrix(self, pc: &mut Pc) -> RMatrix;
}

impl Faer2RMatrix for MatRef<'_, f64> {
    fn to_rmatrix(self, pc: &mut Pc) -> RMatrix {
        let nr = self.nrows();
        let nc = self.ncols();
        let (result, result_slice) = RMatrix::new_double(nr, nc, pc);
        for (k, r) in result_slice.iter_mut().enumerate() {
            *r = self.read(k % nr, k / nc);
        }
        result
    }
}
