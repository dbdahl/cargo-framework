//! Extension Framework for R using Rust

#![allow(dead_code)]

// Helpful resources:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals
//   https://www.tidyverse.org/blog/2019/05/resource-cleanup-in-c-and-the-r-api
//   https://github.com/wch/r-source

use crate::pc::Pc;
use crate::rbindings::*;

use std::ffi::{c_char, CStr};
use std::marker::PhantomData;
use std::str::Utf8Error;

pub struct RObject<RType = AnyType, RMode = Unspecified> {
    pub sexp: SEXP,
    rtype: PhantomData<(RType, RMode)>,
}

pub struct AnyType(());
pub struct Vector(());
pub struct Matrix(());
pub struct Array(());
pub struct Function(());
pub struct Single(());
pub struct Unspecified(());
pub trait Sliceable {}

impl Sliceable for Vector {}
impl Sliceable for Matrix {}
impl Sliceable for Array {}

impl<RType, RMode> RObject<RType, RMode> {
    fn new<RTypeTo, RModeTo>(sexp: SEXP) -> RObject<RTypeTo, RModeTo> {
        RObject {
            sexp,
            rtype: PhantomData,
        }
    }

    fn new_vector<T>(code: u32, length: usize, pc: &mut Pc) -> RObject<Vector, T> {
        Self::new(pc.protect(unsafe { Rf_allocVector(code, length.try_into().unwrap()) }))
    }

    pub fn new_vector_f64(length: usize, pc: &mut Pc) -> RObject<Vector, f64> {
        Self::new_vector::<f64>(REALSXP, length, pc)
    }

    pub fn new_vector_i32(length: usize, pc: &mut Pc) -> RObject<Vector, i32> {
        Self::new_vector::<i32>(INTSXP, length, pc)
    }

    fn new_matrix<T>(code: u32, nrows: usize, ncols: usize, pc: &mut Pc) -> RObject<Matrix, T> {
        Self::new(pc.protect(unsafe {
            Rf_allocMatrix(code, nrows.try_into().unwrap(), ncols.try_into().unwrap())
        }))
    }

    pub fn new_matrix_f64(nrows: usize, ncols: usize, pc: &mut Pc) -> RObject<Matrix, f64> {
        Self::new_matrix::<f64>(REALSXP, nrows, ncols, pc)
    }

    pub fn new_matrix_i32(nrows: usize, ncols: usize, pc: &mut Pc) -> RObject<Matrix, i32> {
        Self::new_matrix::<i32>(INTSXP, nrows, ncols, pc)
    }

    /*
        fn new_array<T>(code: u32, dim: [], pc: &mut Pc) -> RObject<Matrix, T> {
            Self::new(pc.protect(unsafe {
                let dim =
                Rf_allocArray(code, dim)
            }))
        }

        fn new_array_f64(nrows: usize, ncols: usize, pc: &mut Pc) -> RObject<Matrix, f64> {
            Self::new_array::<f64>(REALSXP, nrows, ncols, pc)
        }

        fn new_array_i32(nrows: usize, ncols: usize, pc: &mut Pc) -> RObject<Matrix, i32> {
            Self::new_array::<i32>(INTSXP, nrows, ncols, pc)
        }
    */

    fn convert<RTypeTo, RModeTo>(&self) -> RObject<RTypeTo, RModeTo> {
        Self::new(self.sexp)
    }

    pub fn to_base(&self) -> RObject {
        self.convert()
    }

    pub fn is_f64(&self) -> bool {
        unsafe { Rf_isReal(self.sexp) != 0 }
    }

    pub fn is_i32(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp) != 0 }
    }

    pub fn is_vector(&self) -> bool {
        unsafe { Rf_isVector(self.sexp) != 0 }
    }

    pub fn is_vector_atomic(&self) -> bool {
        unsafe { Rf_isVectorAtomic(self.sexp) != 0 }
    }

    pub fn is_vector_list(&self) -> bool {
        unsafe { Rf_isVectorList(self.sexp) != 0 }
    }

    pub fn is_matrix(&self) -> bool {
        unsafe { Rf_isMatrix(self.sexp) != 0 }
    }

    pub fn is_array(&self) -> bool {
        unsafe { Rf_isArray(self.sexp) != 0 }
    }

    pub fn is_function(&self) -> bool {
        unsafe { Rf_isFunction(self.sexp) != 0 }
    }

    pub fn is_number(&self) -> bool {
        unsafe { Rf_isNumber(self.sexp) != 0 }
    }

    pub fn is_scalar(&self) -> bool {
        if self.is_number() {
            unsafe { Rf_xlength(self.sexp) == 1 }
        } else {
            false
        }
    }

    pub fn as_f64(&self) -> Result<f64, &'static str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asReal(self.sexp) })
        } else {
            Err("Canot be interperated as an f64")
        }
    }

    pub fn as_i32(&self) -> Result<i32, &'static str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asInteger(self.sexp) })
        } else {
            Err("Canot be interperated as an i32")
        }
    }

    pub fn as_usize(&self) -> Result<usize, &'static str> {
        if self.is_scalar() {
            let value = unsafe { Rf_asInteger(self.sexp) };
            match usize::try_from(value) {
                Ok(e) => Ok(e),
                _ => Err("Canot be interperated as a usize scalar"),
            }
        } else {
            Err("Canot be interperated as a usize")
        }
    }

    pub fn as_bool(&self) -> Result<bool, &'static str> {
        if self.is_scalar() {
            let value = unsafe { Rf_asLogical(self.sexp) };
            Ok(value != 0)
        } else {
            Err("Canot be interperated as an bool")
        }
    }

    pub fn as_vector_f64(&self) -> Result<RObject<Vector, f64>, &'static str> {
        if self.is_vector_atomic() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn to_vector_f64(&self, pc: &mut Pc) -> Result<RObject<Vector, f64>, &'static str> {
        if self.is_vector_atomic() {
            if self.is_f64() {
                Ok(self.convert())
            } else if self.is_i32() {
                let x: RObject<Vector, i32> = self.convert();
                Ok(x.to_f64(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn as_vector_i32(&self) -> Result<RObject<Vector, i32>, &'static str> {
        if self.is_vector_atomic() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 vector")
        }
    }

    pub fn to_vector_i32(&self, pc: &mut Pc) -> Result<RObject<Vector, i32>, &'static str> {
        if self.is_vector_atomic() {
            if self.is_i32() {
                Ok(self.convert())
            } else if self.is_f64() {
                let x: RObject<Vector, f64> = self.convert();
                Ok(x.to_i32(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn as_matrix_f64(&self) -> Result<RObject<Matrix, f64>, &'static str> {
        if self.is_matrix() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 matrix")
        }
    }

    pub fn to_matrix_f64(&self, pc: &mut Pc) -> Result<RObject<Matrix, f64>, &'static str> {
        if self.is_matrix() {
            if self.is_f64() {
                Ok(self.convert())
            } else if self.is_i32() {
                let x: RObject<Matrix, i32> = self.convert();
                Ok(x.to_f64(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn as_matrix_i32(&self) -> Result<RObject<Matrix, i32>, &'static str> {
        if self.is_matrix() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 matrix")
        }
    }

    pub fn to_matrix_i32(&self, pc: &mut Pc) -> Result<RObject<Matrix, i32>, &'static str> {
        if self.is_matrix() {
            if self.is_i32() {
                Ok(self.convert())
            } else if self.is_f64() {
                let x: RObject<Matrix, f64> = self.convert();
                Ok(x.to_i32(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn as_array_f64(&self) -> Result<RObject<Array, f64>, &'static str> {
        if self.is_array() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 array")
        }
    }

    pub fn as_array_i32(&self) -> Result<RObject<Array, i32>, &'static str> {
        if self.is_array() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 array")
        }
    }

    pub fn as_vector_list(&self) -> Result<RObject<Vector, Unspecified>, &'static str> {
        if self.is_vector_list() {
            Ok(self.convert())
        } else {
            Err("Not a vector list")
        }
    }

    pub fn as_function(&self) -> Result<RObject<Function, Unspecified>, &'static str> {
        if self.is_function() {
            Ok(self.convert())
        } else {
            Err("Not a function")
        }
    }

    /// Define a new symbol.
    pub fn new_symbol(x: &str, pc: &mut Pc) -> RObject {
        let sexp = pc.protect(unsafe {
            Rf_mkCharLenCE(
                x.as_ptr() as *const c_char,
                x.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            )
        });
        Self::new(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    /// Get an attribute.
    pub fn get_attribute(&self, which: &str, pc: &mut Pc) -> RObject {
        Self::new(unsafe { Rf_getAttrib(self.sexp, Self::new_symbol(which, pc).sexp) })
    }

    /// Set an attribute.
    pub fn set_attribute(&self, which: &str, value: impl Into<RObject<RType, RMode>>, pc: &mut Pc) {
        unsafe {
            Rf_setAttrib(
                self.sexp,
                Self::new_symbol(which, pc).sexp,
                value.into().sexp,
            );
        }
    }
}

impl<S: Sliceable, T> RObject<S, T> {
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    fn slice_engine<U>(&self, data: *mut U) -> &'static [U] {
        let len = self.len();
        unsafe { std::slice::from_raw_parts_mut(data, len) }
    }
}

impl<S: Sliceable> RObject<S, f64> {
    pub fn slice(&self) -> &'static [f64] {
        self.slice_engine(unsafe { REAL(self.sexp) })
    }
}

impl<S: Sliceable> RObject<S, i32> {
    pub fn slice(&self) -> &'static [i32] {
        self.slice_engine(unsafe { INTEGER(self.sexp) })
    }
}

impl<T> RObject<Matrix, T> {
    pub fn nrows(&self) -> usize {
        unsafe { Rf_nrows(self.sexp).try_into().unwrap() }
    }
    pub fn ncols(&self) -> usize {
        unsafe { Rf_ncols(self.sexp).try_into().unwrap() }
    }
}

impl<T> RObject<Array, T> {
    pub fn dim(&self) -> Vec<usize> {
        unimplemented!()
    }
}

impl RObject<Function, Unspecified> {
    fn eval(expression: SEXP, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = pc.protect(expression);
        let mut p_out_error: i32 = 0;
        let sexp = pc.protect(unsafe {
            R_tryEval(expression, R_GetCurrentEnv(), &mut p_out_error as *mut i32)
        });
        match p_out_error {
            0 => {
                let robject = RObject {
                    sexp,
                    rtype: PhantomData,
                };
                Ok(robject)
            }
            e => Err(e),
        }
    }

    pub fn call0(&self, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang1(self.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call1<T1, M1>(&self, arg1: RObject<T1, M1>, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp, arg1.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call2<T1, M1, T2, M2>(
        &self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp, arg1.sexp, arg2.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call3<T1, M1, T2, M2, T3, M3>(
        &self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang4(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call4<T1, M1, T2, M2, T3, M3, T4, M4>(
        &self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        arg4: RObject<T4, M4>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang5(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp, arg4.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call5<T1, M1, T2, M2, T3, M3, T4, M4, T5, M5>(
        &self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        arg4: RObject<T4, M4>,
        arg5: RObject<T5, M5>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe {
            Rf_lang6(
                self.sexp, arg1.sexp, arg2.sexp, arg3.sexp, arg4.sexp, arg5.sexp,
            )
        };
        Self::eval(expression, pc)
    }
}

impl RObject<Vector, f64> {
    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        Self::new(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn get(&self, index: usize) -> f64 {
        unsafe { REAL_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set<RType, RMode>(&self, index: usize, value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, i32> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        Self::new(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn get(&self, index: usize) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set<RType, RMode>(&self, index: usize, value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, &str> {
    pub fn get(&self, index: usize) -> Result<&'static str, Utf8Error> {
        let sexp = unsafe { STRING_ELT(self.sexp, index.try_into().unwrap()) };
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
        c_str.to_str()
    }

    pub fn set<RType, RMode>(&self, index: usize, value: &'static str) {
        unsafe {
            let value = Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            );
            SET_STRING_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, Unspecified> {
    pub fn get(&self, index: usize) -> RObject {
        Self::new(unsafe { VECTOR_ELT(self.sexp, index.try_into().unwrap()) })
    }

    pub fn set<RType, RMode>(&self, index: usize, value: RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, index.try_into().unwrap(), value.sexp);
        }
    }
}

impl<RMode> RObject<Matrix, RMode> {
    fn index(&self, (i, j): (usize, usize)) -> isize {
        let nrows = self.nrows();
        (nrows * j + i).try_into().unwrap()
    }
}

impl RObject<Matrix, f64> {
    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Matrix, i32> {
        Self::new(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn get(&self, index: (usize, usize)) -> f64 {
        unsafe { REAL_ELT(self.sexp, self.index(index)) }
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, i32> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Matrix, f64> {
        Self::new(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn get(&self, index: (usize, usize)) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, self.index(index)) }
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, &str> {
    pub fn get(&self, index: (usize, usize)) -> Result<&'static str, Utf8Error> {
        let sexp = unsafe { STRING_ELT(self.sexp, self.index(index)) };
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
        c_str.to_str()
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: &'static str) {
        unsafe {
            let value = Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            );
            SET_STRING_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, Unspecified> {
    pub fn get(&self, index: (usize, usize)) -> RObject {
        Self::new(unsafe { VECTOR_ELT(self.sexp, self.index(index)) })
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, self.index(index), value.sexp);
        }
    }
}

trait IntoProtected<T> {
    fn into(self, pc: &mut Pc) -> T;
}

impl IntoProtected<RObject<Vector, f64>> for f64 {
    fn into(self, pc: &mut Pc) -> RObject<Vector, f64> {
        RObject::<Vector, f64>::new(pc.protect(unsafe { Rf_ScalarReal(self) }))
    }
}

impl IntoProtected<RObject<Vector, i32>> for i32 {
    fn into(self, pc: &mut Pc) -> RObject<Vector, i32> {
        RObject::<Vector, i32>::new(pc.protect(unsafe { Rf_ScalarInteger(self) }))
    }
}

impl IntoProtected<RObject<Vector, &'static str>> for &'static str {
    fn into(self, pc: &mut Pc) -> RObject<Vector, &'static str> {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                self.as_ptr() as *const c_char,
                self.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            ))
        };
        RObject::<Vector, &'static str>::new(pc.protect(sexp))
    }
}

impl From<RObject<Vector, f64>> for f64 {
    fn from(x: RObject<Vector, f64>) -> Self {
        unsafe { Rf_asReal(x.sexp) }
    }
}

impl From<RObject<Vector, i32>> for i32 {
    fn from(x: RObject<Vector, i32>) -> Self {
        unsafe { Rf_asInteger(x.sexp) }
    }
}
