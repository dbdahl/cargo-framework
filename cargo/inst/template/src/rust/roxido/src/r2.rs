//! Extension Framework for R using Rust

//#![allow(dead_code)]

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

pub struct R {}
pub struct Str;

pub struct AnyType(());
pub struct Vector(());
pub struct Matrix(());
pub struct Array(());
pub struct Function(());
pub struct Unspecified(());
pub trait Sliceable {}

impl Sliceable for Vector {}
impl Sliceable for Matrix {}
impl Sliceable for Array {}

impl R {
    fn wrap<RTypeTo, RModeTo>(sexp: SEXP) -> RObject<RTypeTo, RModeTo> {
        RObject {
            sexp,
            rtype: PhantomData,
        }
    }

    fn new_vector<T>(code: u32, length: usize, pc: &mut Pc) -> RObject<Vector, T> {
        Self::wrap(pc.protect(unsafe { Rf_allocVector(code, length.try_into().unwrap()) }))
    }

    pub fn new_vector_f64(length: usize, pc: &mut Pc) -> RObject<Vector, f64> {
        Self::new_vector::<f64>(REALSXP, length, pc)
    }

    pub fn new_vector_i32(length: usize, pc: &mut Pc) -> RObject<Vector, i32> {
        Self::new_vector::<i32>(INTSXP, length, pc)
    }

    pub fn new_vector_u8(length: usize, pc: &mut Pc) -> RObject<Vector, u8> {
        Self::new_vector::<u8>(RAWSXP, length, pc)
    }

    pub fn new_vector_bool(length: usize, pc: &mut Pc) -> RObject<Vector, bool> {
        Self::new_vector::<bool>(LGLSXP, length, pc)
    }

    pub fn new_vector_str(length: usize, pc: &mut Pc) -> RObject<Vector, Str> {
        Self::new_vector(STRSXP, length, pc)
    }

    fn new_matrix<T>(code: u32, nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, T> {
        Self::wrap(pc.protect(unsafe {
            Rf_allocMatrix(code, nrow.try_into().unwrap(), ncol.try_into().unwrap())
        }))
    }

    pub fn new_matrix_f64(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, f64> {
        Self::new_matrix::<f64>(REALSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_i32(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, i32> {
        Self::new_matrix::<i32>(INTSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_u8(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, u8> {
        Self::new_matrix::<u8>(RAWSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_bool(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, bool> {
        Self::new_matrix::<bool>(LGLSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_str(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, Str> {
        Self::new_matrix::<Str>(STRSXP, nrow, ncol, pc)
    }

    fn new_array<T>(code: u32, dim: &[usize], pc: &mut Pc) -> RObject<Array, T> {
        let dim = dim.to_r(pc);
        Self::wrap(pc.protect(unsafe { Rf_allocArray(code, dim.sexp) }))
    }

    pub fn new_array_f64(dim: &[usize], pc: &mut Pc) -> RObject<Array, f64> {
        Self::new_array::<f64>(REALSXP, dim, pc)
    }

    pub fn new_array_i32(dim: &[usize], pc: &mut Pc) -> RObject<Array, i32> {
        Self::new_array::<i32>(INTSXP, dim, pc)
    }

    pub fn new_array_u8(dim: &[usize], pc: &mut Pc) -> RObject<Array, u8> {
        Self::new_array::<u8>(RAWSXP, dim, pc)
    }

    pub fn new_array_bool(dim: &[usize], pc: &mut Pc) -> RObject<Array, bool> {
        Self::new_array::<bool>(LGLSXP, dim, pc)
    }

    pub fn new_array_str(dim: &[usize], pc: &mut Pc) -> RObject<Array, Str> {
        Self::new_array::<Str>(STRSXP, dim, pc)
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
        R::wrap(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    pub fn null() -> RObject {
        Self::wrap(unsafe { R_NilValue })
    }

    pub fn positive_infinity() -> f64 {
        unsafe { R_PosInf }
    }

    pub fn negative_infinity() -> f64 {
        unsafe { R_NegInf }
    }

    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    pub fn na_f64() -> f64 {
        unsafe { R_NaReal }
    }

    pub fn na_i32() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn na_bool() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
    }

    pub fn is_na(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    pub fn is_finite(x: f64) -> bool {
        unsafe { R_finite(x) != 0 }
    }
}

pub struct RObject<RType = AnyType, RMode = Unspecified> {
    pub sexp: SEXP,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {
    fn convert<RTypeTo, RModeTo>(&self) -> RObject<RTypeTo, RModeTo> {
        R::wrap(self.sexp)
    }

    pub fn to_base(&self) -> RObject {
        self.convert()
    }

    /// Duplicate an object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    /// E.g, call this method on arguments pass via `.Call` before modifying them.
    ///
    pub fn duplicate(&self, pc: &mut Pc) -> RObject<RType, RMode> {
        R::wrap(pc.protect(unsafe { Rf_duplicate(self.sexp) }))
    }

    /// Get an attribute.
    pub fn get_attribute(&self, which: &str, pc: &mut Pc) -> RObject {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R::new_symbol(which, pc).sexp) })
    }

    /// Set an attribute.
    pub fn set_attribute(&self, which: &str, value: RObject<RType, RMode>, pc: &mut Pc) {
        unsafe {
            Rf_setAttrib(self.sexp, R::new_symbol(which, pc).sexp, value.sexp);
        }
    }

    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp) != 0 }
    }

    pub fn is_f64(&self) -> bool {
        unsafe { Rf_isReal(self.sexp) != 0 }
    }

    pub fn is_i32(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp) != 0 }
    }

    pub fn is_u8(&self) -> bool {
        unsafe { TYPEOF(self.sexp) == RAWSXP as i32 }
    }

    pub fn is_bool(&self) -> bool {
        unsafe { Rf_isLogical(self.sexp) != 0 }
    }

    pub fn is_str(&self) -> bool {
        unsafe { Rf_isString(self.sexp) != 0 }
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

    pub fn as_f64(&self) -> Result<f64, &str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asReal(self.sexp) })
        } else {
            Err("Cannot be interperated as an f64")
        }
    }

    pub fn as_i32(&self) -> Result<i32, &str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asInteger(self.sexp) })
        } else {
            Err("Cannot be interperated as an i32")
        }
    }

    pub fn as_usize(&self) -> Result<usize, &str> {
        if self.is_scalar() {
            let value = unsafe { Rf_asInteger(self.sexp) };
            match usize::try_from(value) {
                Ok(e) => Ok(e),
                _ => Err("Cannot be interperated as a usize scalar"),
            }
        } else {
            Err("Cannot be interperated as a usize")
        }
    }

    pub fn as_bool(&self) -> Result<bool, &str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asLogical(self.sexp) != 0 })
        } else {
            Err("Cannot be interperated as an bool")
        }
    }

    pub fn as_vector_f64(&self) -> Result<RObject<Vector, f64>, &str> {
        if self.is_vector_atomic() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn to_vector_f64(&self, pc: &mut Pc) -> Result<RObject<Vector, f64>, &str> {
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

    pub fn as_vector_i32(&self) -> Result<RObject<Vector, i32>, &str> {
        if self.is_vector_atomic() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 vector")
        }
    }

    pub fn to_vector_i32(&self, pc: &mut Pc) -> Result<RObject<Vector, i32>, &str> {
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

    pub fn as_vector_u8(&self) -> Result<RObject<Vector, u8>, &str> {
        if self.is_vector_atomic() && self.is_u8() {
            Ok(self.convert())
        } else {
            Err("Not a u8 vector")
        }
    }

    pub fn as_vector_bool(&self) -> Result<RObject<Vector, bool>, &str> {
        if self.is_vector_atomic() && self.is_bool() {
            Ok(self.convert())
        } else {
            Err("Not a bool vector")
        }
    }

    pub fn to_vector_bool(&self, pc: &mut Pc) -> Result<RObject<Vector, bool>, &str> {
        if self.is_vector_atomic() {
            if self.is_bool() {
                Ok(self.convert())
            } else if self.is_f64() {
                let x: RObject<Vector, f64> = self.convert();
                Ok(x.to_bool(pc))
            } else if self.is_i32() {
                let x: RObject<Vector, i32> = self.convert();
                Ok(x.to_bool(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn as_matrix_f64(&self) -> Result<RObject<Matrix, f64>, &str> {
        if self.is_matrix() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 matrix")
        }
    }

    pub fn to_matrix_f64(&self, pc: &mut Pc) -> Result<RObject<Matrix, f64>, &str> {
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

    pub fn as_matrix_i32(&self) -> Result<RObject<Matrix, i32>, &str> {
        if self.is_matrix() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 matrix")
        }
    }

    pub fn to_matrix_i32(&self, pc: &mut Pc) -> Result<RObject<Matrix, i32>, &str> {
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

    pub fn as_matrix_u8(&self) -> Result<RObject<Matrix, u8>, &str> {
        if self.is_matrix() && self.is_u8() {
            Ok(self.convert())
        } else {
            Err("Not a u8 matrix")
        }
    }

    pub fn as_matrix_bool(&self) -> Result<RObject<Matrix, bool>, &str> {
        if self.is_matrix() && self.is_bool() {
            Ok(self.convert())
        } else {
            Err("Not a bool matrix")
        }
    }

    pub fn to_matrix_bool(&self, pc: &mut Pc) -> Result<RObject<Matrix, bool>, &str> {
        if self.is_matrix() {
            if self.is_bool() {
                Ok(self.convert())
            } else if self.is_f64() {
                let x: RObject<Matrix, f64> = self.convert();
                Ok(x.to_bool(pc))
            } else if self.is_i32() {
                let x: RObject<Matrix, i32> = self.convert();
                Ok(x.to_bool(pc))
            } else {
                Err("Does not contain i32 or f64")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn as_array_f64(&self) -> Result<RObject<Array, f64>, &str> {
        if self.is_array() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 array")
        }
    }

    pub fn as_array_i32(&self) -> Result<RObject<Array, i32>, &str> {
        if self.is_array() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 array")
        }
    }

    pub fn as_array_u8(&self) -> Result<RObject<Array, u8>, &str> {
        if self.is_array() && self.is_u8() {
            Ok(self.convert())
        } else {
            Err("Not a u8 array")
        }
    }

    pub fn as_array_bool(&self) -> Result<RObject<Array, bool>, &str> {
        if self.is_array() && self.is_bool() {
            Ok(self.convert())
        } else {
            Err("Not a bool array")
        }
    }

    pub fn as_vector_list(&self) -> Result<RObject<Vector, Unspecified>, &str> {
        if self.is_vector_list() {
            Ok(self.convert())
        } else {
            Err("Not a vector list")
        }
    }

    pub fn as_function(&self) -> Result<RObject<Function, Unspecified>, &str> {
        if self.is_function() {
            Ok(self.convert())
        } else {
            Err("Not a function")
        }
    }
}

impl<S: Sliceable, T> RObject<S, T> {
    pub fn is_empty(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 0 }
    }
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    fn slice_engine<U>(&self, data: *mut U) -> &'static mut [U] {
        let len = self.len();
        unsafe { std::slice::from_raw_parts_mut(data, len) }
    }
}

impl<S: Sliceable> RObject<S, f64> {
    pub fn slice(&self) -> &'static mut [f64] {
        self.slice_engine(unsafe { REAL(self.sexp) })
    }
}

impl<S: Sliceable> RObject<S, i32> {
    pub fn slice(&self) -> &'static mut [i32] {
        self.slice_engine(unsafe { INTEGER(self.sexp) })
    }
}

impl<S: Sliceable> RObject<S, u8> {
    pub fn slice(&self) -> &'static mut [u8] {
        self.slice_engine(unsafe { RAW(self.sexp) })
    }
}

impl<S: Sliceable> RObject<S, bool> {
    pub fn slice(&self) -> &'static mut [i32] {
        self.slice_engine(unsafe { LOGICAL(self.sexp) })
    }
}

impl<T> RObject<Matrix, T> {
    pub fn nrow(&self) -> usize {
        unsafe { Rf_nrows(self.sexp).try_into().unwrap() }
    }
    pub fn ncol(&self) -> usize {
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

impl<RMode> RObject<Vector, RMode> {
    pub fn get_names(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        self.get_attribute("names", pc).convert()
    }

    pub fn set_names(&self, names: RObject<Vector, Str>) -> Result<(), &str> {
        if unsafe { Rf_length(names.sexp) != Rf_length(self.sexp) } {
            return Err("Lengths do not match");
        }
        unsafe {
            Rf_namesgets(self.sexp, names.sexp);
        }
        Ok(())
    }
}

impl RObject<Vector, f64> {
    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn to_bool(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    pub fn get(&self, index: usize) -> f64 {
        unsafe { REAL_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set(&self, index: usize, value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, i32> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_bool(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    pub fn get(&self, index: usize) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set(&self, index: usize, value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, u8> {
    pub fn get(&self, index: usize) -> u8 {
        unsafe { RAW_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set(&self, index: usize, value: u8) {
        unsafe {
            SET_RAW_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, bool> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn get(&self, index: usize) -> bool {
        unsafe { LOGICAL_ELT(self.sexp, index.try_into().unwrap()) != 0 }
    }

    pub fn get_i32(&self, index: usize) -> i32 {
        unsafe { LOGICAL_ELT(self.sexp, index.try_into().unwrap()) }
    }

    pub fn set(&self, index: usize, value: bool) {
        unsafe {
            SET_LOGICAL_ELT(
                self.sexp,
                index.try_into().unwrap(),
                if value { 1 } else { 0 },
            );
        }
    }

    pub fn set_i32(&self, index: usize, value: i32) {
        unsafe {
            SET_LOGICAL_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, Str> {
    pub fn get(&self, index: usize) -> Result<&str, Utf8Error> {
        let sexp = unsafe { STRING_ELT(self.sexp, index.try_into().unwrap()) };
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
        c_str.to_str()
    }

    pub fn set(&self, index: usize, value: &str) {
        unsafe {
            let value = Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            );
            SET_STRING_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }

    pub fn set_na(&self, index: usize) {
        unsafe {
            SET_STRING_ELT(self.sexp, index.try_into().unwrap(), R_NaString);
        }
    }
}

impl RObject<Vector, Unspecified> {
    pub fn get(&self, index: usize) -> RObject {
        R::wrap(unsafe { VECTOR_ELT(self.sexp, index.try_into().unwrap()) })
    }

    pub fn set<RType, RMode>(&self, index: usize, value: RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, index.try_into().unwrap(), value.sexp);
        }
    }
}

impl<RMode> RObject<Matrix, RMode> {
    pub fn index(&self, (i, j): (usize, usize)) -> isize {
        let nrow = self.nrow();
        (nrow * j + i).try_into().unwrap()
    }

    pub fn get_dimnames(&self, pc: &mut Pc) -> RObject<Vector, Unspecified> {
        self.get_attribute("dimnames", pc).convert()
    }

    pub fn set_dimnames(&self, names: RObject<Vector, Unspecified>) -> Result<(), &str> {
        if !names.is_vector_list() {
            return Err("Not a list");
        }
        if names.len() != 2 {
            return Err("Length should be two");
        }
        let rownames = names.get(0);
        if !rownames.is_vector_atomic() || !rownames.is_str() {
            return Err("Row names must be a character vector");
        }
        let rownames: RObject<Vector, Str> = rownames.convert();
        if rownames.len() != self.nrow() {
            return Err("Row names do not match the number of rows");
        }
        let colnames = names.get(1);
        if !colnames.is_vector_atomic() || !colnames.is_str() {
            return Err("Column names must be a character vector");
        }
        let colnames: RObject<Vector, Str> = colnames.convert();
        if colnames.len() != self.ncol() {
            return Err("Column names do not match the number of columns");
        }
        unsafe {
            Rf_namesgets(self.sexp, names.sexp);
        }
        Ok(())
    }
}

impl RObject<Matrix, f64> {
    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Matrix, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn to_bool(&self, pc: &mut Pc) -> RObject<Matrix, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    pub fn get(&self, index: (usize, usize)) -> f64 {
        unsafe { REAL_ELT(self.sexp, self.index(index)) }
    }

    pub fn set(&self, index: (usize, usize), value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, i32> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Matrix, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_bool(&self, pc: &mut Pc) -> RObject<Matrix, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    pub fn get(&self, index: (usize, usize)) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, self.index(index)) }
    }

    pub fn set(&self, index: (usize, usize), value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, u8> {
    pub fn get(&self, index: (usize, usize)) -> u8 {
        unsafe { RAW_ELT(self.sexp, self.index(index)) }
    }

    pub fn set(&self, index: (usize, usize), value: u8) {
        unsafe {
            SET_RAW_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, bool> {
    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Matrix, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Matrix, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn get(&self, index: (usize, usize)) -> bool {
        unsafe { LOGICAL_ELT(self.sexp, self.index(index)) != 0 }
    }

    pub fn get_i32(&self, index: (usize, usize)) -> i32 {
        unsafe { LOGICAL_ELT(self.sexp, self.index(index)) }
    }

    pub fn set(&self, index: (usize, usize), value: bool) {
        unsafe {
            SET_LOGICAL_ELT(self.sexp, self.index(index), if value { 1 } else { 0 });
        }
    }

    pub fn set_i32(&self, index: (usize, usize), value: i32) {
        unsafe {
            SET_LOGICAL_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, Str> {
    pub fn get(&self, index: (usize, usize)) -> Result<&str, Utf8Error> {
        let sexp = unsafe { STRING_ELT(self.sexp, self.index(index)) };
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
        c_str.to_str()
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: &str) {
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
        R::wrap(unsafe { VECTOR_ELT(self.sexp, self.index(index)) })
    }

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, self.index(index), value.sexp);
        }
    }
}

trait IntoR<T> {
    fn to_r(&self, pc: &mut Pc) -> T;
}

impl IntoR<RObject<Vector, f64>> for f64 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_ScalarReal(*self) }))
    }
}

impl IntoR<RObject<Vector, i32>> for i32 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger(*self) }))
    }
}

impl IntoR<RObject<Vector, u8>> for u8 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        R::wrap(pc.protect(unsafe { Rf_ScalarRaw(*self) }))
    }
}

impl IntoR<RObject<Vector, bool>> for bool {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe { Rf_ScalarLogical(if *self { 1 } else { 0 }) }))
    }
}

impl IntoR<RObject<Vector, Str>> for &str {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        let sexp = unsafe {
            Rf_ScalarString(Rf_mkCharLenCE(
                self.as_ptr() as *const c_char,
                self.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            ))
        };
        R::wrap(pc.protect(sexp))
    }
}

impl<const N: usize> IntoR<RObject<Vector, f64>> for [f64; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, f64>> for &[f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> IntoR<RObject<Vector, i32>> for [i32; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, i32>> for &[i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> IntoR<RObject<Vector, u8>> for [u8; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, u8>> for &[u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> IntoR<RObject<Vector, i32>> for [usize; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, i32>> for &[usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<const N: usize> IntoR<RObject<Vector, bool>> for [bool; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, bool>> for &[bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<const N: usize> IntoR<RObject<Vector, Str>> for [&str; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        self.as_ref().to_r(pc)
    }
}

impl IntoR<RObject<Vector, Str>> for &[&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        let result = R::new_vector_str(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            result.set(index, s);
        }
        result
    }
}

// Support raw
