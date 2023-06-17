use faer_core::MatRef;
use roxido::{Pc, Rval};

pub trait RvalFaer {
    fn as_matrix_f64(self) -> Result<MatRef<'static, f64>, &'static str>;
}

impl RvalFaer for Rval {
    fn as_matrix_f64(self) -> Result<MatRef<'static, f64>, &'static str> {
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

pub trait RvalMatRef {
    fn to_rval(self, pc: &mut Pc) -> Rval;
}

impl RvalMatRef for MatRef<'_, f64> {
    fn to_rval(self, pc: &mut Pc) -> Rval {
        let nr = self.nrows();
        let nc = self.ncols();
        let (result_rval, result_slice) = Rval::new_matrix_double(nr, nc, pc);
        for (k, r) in result_slice.iter_mut().enumerate() {
            *r = self.read(k % nr, k / nc);
        }
        result_rval
    }
}
