use faer_core::MatRef;
use roxido::*;

pub trait FaerFromRMatrix {
    fn as_raer_f64(&self) -> Result<MatRef<'static, f64>, &'static str>;
}

impl FaerFromRMatrix for RMatrix {
    fn as_raer_f64(&self) -> Result<MatRef<'static, f64>, &'static str> {
        Ok(unsafe {
            let slice = self.slice_double()?;
            let nrow = self.nrow();
            let ncol = self.ncol();
            MatRef::from_raw_parts(slice.as_ptr(), nrow, ncol, 1, nrow.try_into().unwrap())
        })
    }
}

pub trait RMatrixFromMatRef {
    fn allocate(self, pc: &mut Pc) -> RMatrix;
}

impl RMatrixFromMatRef for MatRef<'_, f64> {
    fn allocate(self, pc: &mut Pc) -> RMatrix {
        let nr = self.nrows();
        let nc = self.ncols();
        let (result, slice) = RMatrix::new_double(nr, nc, pc);
        for (k, r) in slice.iter_mut().enumerate() {
            *r = self.read(k % nr, k / nc);
        }
        result
    }
}
