use faer_core::MatRef;
use roxido::r::Matrix;
use roxido::*;

pub trait RMatrix2Faer {
    fn as_faer(self) -> Result<MatRef<'static, f64>, &'static str>;
}

impl RMatrix2Faer for RObject<Matrix, f64> {
    fn as_faer(self) -> Result<MatRef<'static, f64>, &'static str> {
        Ok({
            let nrow = self.nrow();
            unsafe {
                MatRef::from_raw_parts(
                    self.slice().as_ptr(),
                    nrow,
                    self.ncol(),
                    1,
                    nrow.try_into().unwrap(),
                )
            }
        })
    }
}

pub trait ToR1<S, T> {
    fn to_r(&self, pc: &mut Pc) -> RObject<S, T>;
}

impl ToR1<Matrix, f64> for MatRef<'_, f64> {
    fn to_r(&self, pc: &mut Pc) -> RObject<Matrix, f64> {
        let nr = self.nrows();
        let nc = self.ncols();
        let result = R::new_matrix_double(nr, nc, pc);
        for (k, r) in result.slice().iter_mut().enumerate() {
            *r = self.read(k % nr, k / nc);
        }
        result
    }
}
