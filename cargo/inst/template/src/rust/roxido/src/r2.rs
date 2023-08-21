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

use std::ffi::{c_char, c_void, CStr};
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::Utf8Error;

pub struct R {}
pub struct Str;

pub struct AnyType(());
pub struct Vector(());
pub struct Matrix(());
pub struct Array(());
pub struct Function(());
pub struct ExternalPtr(());
pub struct Unspecified(());
pub trait Sliceable {}

impl Sliceable for Vector {}
impl Sliceable for Matrix {}
impl Sliceable for Array {}

pub trait Convertible {}

impl Convertible for Vector {}
impl Convertible for Matrix {}
impl Convertible for Array {}
impl Convertible for Function {}
impl Convertible for ExternalPtr {}

impl R {
    pub fn from_old(r: crate::r::RObject) -> RObject {
        Self::wrap(r.0)
    }

    pub fn to_old(r: RObject) -> crate::r::RObject {
        crate::r::RObject(r.sexp)
    }

    fn wrap<RTypeTo, RModeTo>(sexp: SEXP) -> RObject<RTypeTo, RModeTo> {
        RObject {
            sexp,
            rtype: PhantomData,
        }
    }

    pub fn new_object(sexp: SEXP) -> RObject {
        Self::wrap(sexp)
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

    pub fn new_vector_list(length: usize, pc: &mut Pc) -> RObject<Vector, Unspecified> {
        Self::new_vector(VECSXP, length, pc)
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

    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new_error(message: &str, pc: &mut Pc) -> RObject {
        let list = Self::new_vector_list(2, pc);
        list.set(0, &message.to_r(pc));
        list.set(1, &Self::null());
        let _ = list.set_names(&["message", "calls"].to_r(pc));
        list.set_class(&["error", "condition"].to_r(pc));
        list.into()
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
        Self::wrap(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    /// Move Rust object to an R external pointer
    ///
    /// This method moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`RObject::decode_as_val`].
    ///
    pub fn encode<T, RType, RMode>(x: T, tag: &RObject<RType, RMode>) -> RObject<ExternalPtr, T> {
        unsafe {
            // Move to Box<_> and then forget about it.
            let ptr = Box::into_raw(Box::new(x)) as *mut c_void;
            Self::wrap(R_MakeExternalPtr(ptr, tag.sexp, R_NilValue))
        }
    }

    pub fn null() -> RObject {
        Self::wrap(unsafe { R_NilValue })
    }

    pub fn infinity_positive() -> f64 {
        unsafe { R_PosInf }
    }

    pub fn infinity_negative() -> f64 {
        unsafe { R_NegInf }
    }

    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
    }

    pub fn new_nan(pc: &mut Pc) -> RObject<Vector, f64> {
        Self::nan().to_r(pc)
    }

    pub fn na_f64() -> f64 {
        unsafe { R_NaReal }
    }

    pub fn new_na_f64(pc: &mut Pc) -> RObject<Vector, f64> {
        Self::na_f64().to_r(pc)
    }

    pub fn is_na_f64(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    pub fn na_i32() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn new_na_i32(pc: &mut Pc) -> RObject<Vector, i32> {
        Self::na_i32().to_r(pc)
    }

    pub fn is_na_i32(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    pub fn na_bool() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn new_na_bool(pc: &mut Pc) -> RObject<Vector, bool> {
        Self::wrap(pc.protect(unsafe { Rf_ScalarLogical(Self::na_bool()) }))
    }

    pub fn is_na_bool(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    pub fn is_finite(x: f64) -> bool {
        unsafe { R_finite(x) != 0 }
    }

    /// Generate random bytes using R's RNG.
    pub fn random_bytes<const LENGTH: usize>() -> [u8; LENGTH] {
        unsafe {
            let m = (u8::MAX as f64) + 1.0;
            let mut bytes: [u8; LENGTH] = [0; LENGTH];
            GetRNGstate();
            for x in bytes.iter_mut() {
                *x = R_unif_index(m) as u8;
            }
            PutRNGstate();
            bytes
        }
    }

    /// Flush the R console.
    pub fn flush_console() {
        unsafe { R_FlushConsole() };
    }

    /// Check to see if the user has attempted to interrupt the execution.
    pub fn check_user_interrupt() -> bool {
        extern "C" fn check_interrupt_fn(_: *mut c_void) {
            unsafe { R_CheckUserInterrupt() };
        }
        unsafe { R_ToplevelExec(Some(check_interrupt_fn), std::ptr::null_mut()) == 0 }
    }
}

#[repr(C)]
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
    pub fn set_attribute<RTypeValue, RModeValue>(
        &self,
        which: &str,
        value: &RObject<RTypeValue, RModeValue>,
        pc: &mut Pc,
    ) {
        unsafe {
            Rf_setAttrib(self.sexp, R::new_symbol(which, pc).sexp, value.sexp);
        }
    }

    pub fn get_class(&self) -> RObject<Vector, Str> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_ClassSymbol) })
    }

    pub fn set_class(&self, names: &RObject<Vector, Str>) {
        unsafe {
            Rf_classgets(self.sexp, names.sexp);
        }
    }

    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp) != 0 }
    }

    pub fn is_number(&self) -> bool {
        unsafe { Rf_isNumber(self.sexp) != 0 }
    }

    pub fn is_scalar(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 1 }
    }

    pub fn is_na(&self) -> bool {
        if self.is_scalar() {
            if self.is_f64() {
                unsafe { R_IsNA(Rf_asReal(self.sexp)) != 0 }
            } else if self.is_i32() {
                unsafe { Rf_asInteger(self.sexp) == R::na_i32() }
            } else if self.is_bool() {
                unsafe { Rf_asLogical(self.sexp) == R::na_bool() }
            } else if self.is_str() {
                unsafe { Rf_asChar(self.sexp) == R_NaString }
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_nan(&self) -> bool {
        if self.is_scalar() && self.is_f64() {
            unsafe { R_IsNaN(Rf_asReal(self.sexp)) != 0 }
        } else {
            false
        }
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

    pub fn is_external_ptr(&self) -> bool {
        unsafe { TYPEOF(self.sexp) == EXTPTRSXP as i32 }
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
            if self.is_i32() {
                Ok(unsafe { Rf_asInteger(self.sexp) })
            } else if self.is_f64() {
                let y = unsafe { Rf_asReal(self.sexp) };
                if y > f64::from(i32::MAX) {
                    Err("Value greater than maximum i32")
                } else if y == f64::from(i32::MIN) {
                    Err("Value equals R's NA for i32")
                } else if y < f64::from(i32::MIN) {
                    Err("Value less than minumum i32")
                } else if y.is_nan() {
                    Err("Value equal R's NaN for f64")
                } else {
                    Ok(y.round() as i32)
                }
            } else if self.is_bool() {
                let y = unsafe { Rf_asLogical(self.sexp) };
                if y == i32::MIN {
                    Err("Value equals R's NA for bool")
                } else {
                    Ok(y)
                }
            } else {
                Err("Cannot be interperated as an i32")
            }
        } else {
            Err("Cannot be interperated as an i32")
        }
    }

    pub fn as_usize(&self) -> Result<usize, &str> {
        if self.is_scalar() {
            if self.is_i32() {
                let y = unsafe { Rf_asInteger(self.sexp) };
                match usize::try_from(y) {
                    Ok(z) => Ok(z),
                    _ => Err("Cannot be interperated as a usize"),
                }
            } else if self.is_f64() {
                let y = unsafe { Rf_asReal(self.sexp) };
                let z = y as usize;
                if z as f64 != y {
                    Err("Cannot be interperated as a usize")
                } else {
                    Ok(z)
                }
            } else if self.is_bool() {
                let y = unsafe { Rf_asLogical(self.sexp) };
                if y == i32::MIN {
                    Err("Value equals R's NA for bool")
                } else {
                    match usize::try_from(y) {
                        Ok(z) => Ok(z),
                        _ => Err("Cannot be interperated as a usize"),
                    }
                }
            } else {
                Err("Cannot be interperated as a usize")
            }
        } else {
            Err("Cannot be interperated as a usize")
        }
    }

    pub fn as_bool(&self) -> Result<bool, &str> {
        if self.is_scalar() {
            if self.is_bool() {
                let y = unsafe { Rf_asLogical(self.sexp) };
                if y == i32::MIN {
                    Err("Value equals R's NA for bool")
                } else {
                    Ok(y != 0)
                }
            } else if self.is_i32() {
                let y = unsafe { Rf_asInteger(self.sexp) };
                if y == i32::MIN {
                    Err("Value equals R's NA for i32")
                } else {
                    Ok(y != 0)
                }
            } else if self.is_f64() {
                let y = unsafe { Rf_asReal(self.sexp) };
                if y.is_nan() {
                    Err("Value equal R's NaN for f64")
                } else if R::is_na_f64(y) {
                    Err("Value equal R's NA for f64")
                } else {
                    Ok(y != 0.0)
                }
            } else {
                Err("Cannot be interperated as a bool")
            }
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

    pub fn as_vector_i32(&self) -> Result<RObject<Vector, i32>, &str> {
        if self.is_vector_atomic() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 vector")
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

    pub fn as_vector_str(&self) -> Result<RObject<Vector, Str>, &str> {
        if self.is_vector_atomic() && self.is_str() {
            Ok(self.convert())
        } else {
            Err("Not a str vector")
        }
    }

    pub fn as_matrix_f64(&self) -> Result<RObject<Matrix, f64>, &str> {
        if self.is_matrix() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 matrix")
        }
    }

    pub fn as_matrix_i32(&self) -> Result<RObject<Matrix, i32>, &str> {
        if self.is_matrix() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 matrix")
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

    pub fn as_external_ptr(&self) -> Result<RObject<ExternalPtr, Unspecified>, &str> {
        if self.is_external_ptr() {
            Ok(self.convert())
        } else {
            Err("Not an external pointer")
        }
    }

    pub fn to_vector_f64(&self, pc: &mut Pc) -> Result<RObject<Vector, f64>, &str> {
        if self.is_vector_atomic() {
            if self.is_f64() {
                Ok(self.convert())
            } else if self.is_i32() || self.is_u8() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn to_vector_i32(&self, pc: &mut Pc) -> Result<RObject<Vector, i32>, &str> {
        if self.is_vector_atomic() {
            if self.is_i32() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_u8() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn to_vector_u8(&self, pc: &mut Pc) -> Result<RObject<Vector, u8>, &str> {
        if self.is_vector_atomic() {
            if self.is_u8() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_i32() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, RAWSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn to_vector_bool(&self, pc: &mut Pc) -> Result<RObject<Vector, bool>, &str> {
        if self.is_vector_atomic() {
            if self.is_bool() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_i32() || self.is_u8() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not an vector")
        }
    }

    pub fn to_matrix_f64(&self, pc: &mut Pc) -> Result<RObject<Matrix, f64>, &str> {
        if self.is_matrix() {
            if self.is_f64() {
                Ok(self.convert())
            } else if self.is_i32() || self.is_u8() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn to_matrix_i32(&self, pc: &mut Pc) -> Result<RObject<Matrix, i32>, &str> {
        if self.is_matrix() {
            if self.is_i32() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_u8() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn to_matrix_u8(&self, pc: &mut Pc) -> Result<RObject<Matrix, u8>, &str> {
        if self.is_matrix() {
            if self.is_u8() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_i32() || self.is_bool() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, RAWSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not a matrix")
        }
    }

    pub fn to_matrix_bool(&self, pc: &mut Pc) -> Result<RObject<Matrix, bool>, &str> {
        if self.is_matrix() {
            if self.is_bool() {
                Ok(self.convert())
            } else if self.is_f64() || self.is_i32() || self.is_u8() {
                Ok(R::wrap(
                    pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }),
                ))
            } else {
                Err("Does not contain i32, f64, u8, or bool")
            }
        } else {
            Err("Not a matrix")
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

    pub fn to_f64(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_i32(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn to_u8(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, RAWSXP) }))
    }

    pub fn to_bool(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
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

    pub fn dim(&self) -> [usize; 2] {
        [self.nrow(), self.ncol()]
    }

    // Create a new vector from a matrix.
    pub fn to_vector(&self) -> RObject<Vector, T> {
        unsafe { Rf_setAttrib(self.sexp, R_DimSymbol, R_NilValue) };
        self.convert()
    }
}

impl<T> RObject<Array, T> {
    pub fn dim(&self) -> Vec<usize> {
        let d = R::wrap::<Vector, i32>(unsafe { Rf_getAttrib(self.sexp, R_DimSymbol) });
        d.slice().iter().map(|&x| x.try_into().unwrap()).collect()
    }

    // Create a new vector from a matrix.
    pub fn to_vector(&self) -> RObject<Vector, T> {
        unsafe { Rf_setAttrib(self.sexp, R_DimSymbol, R_NilValue) };
        self.convert()
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

    pub fn call1<T1, M1>(&self, arg1: &RObject<T1, M1>, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang2(self.sexp, arg1.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call2<T1, M1, T2, M2>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang3(self.sexp, arg1.sexp, arg2.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call3<T1, M1, T2, M2, T3, M3>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T2, M3>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang4(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call4<T1, M1, T2, M2, T3, M3, T4, M4>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T2, M3>,
        arg4: &RObject<T4, M4>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang5(self.sexp, arg1.sexp, arg2.sexp, arg3.sexp, arg4.sexp) };
        Self::eval(expression, pc)
    }

    pub fn call5<T1, M1, T2, M2, T3, M3, T4, M4, T5, M5>(
        &self,
        arg1: &RObject<T1, M1>,
        arg2: &RObject<T2, M2>,
        arg3: &RObject<T2, M3>,
        arg4: &RObject<T4, M4>,
        arg5: &RObject<T5, M5>,
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
    pub fn get_names(&self) -> RObject<Vector, Str> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_NamesSymbol) })
    }

    pub fn set_names(&self, names: &RObject<Vector, Str>) -> Result<(), &str> {
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
                if value {
                    Rboolean_TRUE as i32
                } else {
                    Rboolean_FALSE as i32
                },
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

    pub fn set<RType, RMode>(&self, index: usize, value: &RObject<RType, RMode>) {
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

    pub fn get_dimnames(&self) -> RObject<Vector, Unspecified> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_DimNamesSymbol) })
    }

    pub fn set_dimnames(&self, names: &RObject<Vector, Unspecified>) -> Result<(), &str> {
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
            Rf_dimnamesgets(self.sexp, names.sexp);
        }
        Ok(())
    }
}

impl RObject<Matrix, f64> {
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
    pub fn get(&self, index: (usize, usize)) -> bool {
        unsafe { LOGICAL_ELT(self.sexp, self.index(index)) != 0 }
    }

    pub fn get_i32(&self, index: (usize, usize)) -> i32 {
        unsafe { LOGICAL_ELT(self.sexp, self.index(index)) }
    }

    pub fn set(&self, index: (usize, usize), value: bool) {
        unsafe {
            SET_LOGICAL_ELT(
                self.sexp,
                self.index(index),
                if value {
                    Rboolean_TRUE as i32
                } else {
                    Rboolean_FALSE as i32
                },
            );
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

    pub fn set<RType, RMode>(&self, index: (usize, usize), value: &RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, self.index(index), value.sexp);
        }
    }
}

impl<T> RObject<ExternalPtr, T> {
    /// Get tag for an R external pointer
    ///
    /// This method get the tag associated with an R external pointer, which was set by [`Self::external_pointer_encode`].
    ///
    pub fn tag(&self) -> RObject {
        R::wrap(unsafe { R_ExternalPtrTag(self.sexp) })
    }

    pub fn set_type<ToT>(&self) -> RObject<ExternalPtr, ToT> {
        self.convert()
    }

    /// Move an R external pointer to a Rust object
    ///
    /// This method moves an R external pointer created by [`Self::external_pointer_encode`] to a Rust object and Rust will then manage its memory.
    ///
    pub fn decode_as_val(&self) -> T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
            *Box::from_raw(ptr)
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer
    ///
    /// This method obtained a reference to a Rust object from an R external pointer created by [`Self::external_pointer_encode`].
    ///
    pub fn decode_as_ref(&self) -> &T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer
    ///
    /// This method obtained a mutable reference to a Rust object from an R external pointer created by [`Self::external_pointer_encode`].
    ///
    pub fn decode_as_mut_ref(&self) -> &mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
            ptr.as_mut().unwrap()
        }
    }
}

pub trait ToR1<T> {
    fn to_r(&self, pc: &mut Pc) -> T;
}

pub trait ToR2<T> {
    fn to_r(self, pc: &mut Pc) -> T;
}

pub trait ToR3<T> {
    fn to_r(self, pc: &mut Pc) -> T;
}

pub trait ToR4<T> {
    fn to_r(self, pc: &mut Pc) -> T;
}

impl ToR1<RObject<Vector, f64>> for f64 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_ScalarReal(*self) }))
    }
}

impl ToR1<RObject<Vector, i32>> for i32 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger(*self) }))
    }
}

impl ToR1<RObject<Vector, u8>> for u8 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        R::wrap(pc.protect(unsafe { Rf_ScalarRaw(*self) }))
    }
}

impl ToR1<RObject<Vector, bool>> for bool {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        R::wrap(pc.protect(unsafe {
            Rf_ScalarLogical(if *self {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            })
        }))
    }
}

impl ToR1<RObject<Vector, Str>> for &str {
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

impl<const N: usize> ToR1<RObject<Vector, f64>> for [f64; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, f64>> for &[f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<RObject<Vector, f64>> for &mut [f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> ToR1<RObject<Vector, i32>> for [i32; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, i32>> for &[i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<RObject<Vector, i32>> for &mut [i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> ToR1<RObject<Vector, u8>> for [u8; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, u8>> for &[u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<RObject<Vector, u8>> for &mut [u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<const N: usize> ToR1<RObject<Vector, i32>> for [usize; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, i32>> for &[usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl ToR1<RObject<Vector, i32>> for &mut [usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<const N: usize> ToR1<RObject<Vector, bool>> for [bool; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, bool>> for &[bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl ToR1<RObject<Vector, bool>> for &mut [bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<const N: usize> ToR1<RObject<Vector, Str>> for [&str; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<RObject<Vector, Str>> for &[&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        let result = R::new_vector_str(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            result.set(index, s);
        }
        result
    }
}

impl ToR1<RObject<Vector, Str>> for &mut [&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Str> {
        let result = R::new_vector_str(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            result.set(index, s);
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a f64> + ExactSizeIterator> ToR2<RObject<Vector, f64>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut f64> + ExactSizeIterator> ToR3<RObject<Vector, f64>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = f64> + ExactSizeIterator> ToR4<RObject<Vector, f64>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_f64(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a i32> + ExactSizeIterator> ToR2<RObject<Vector, i32>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a mut i32> + ExactSizeIterator> ToR3<RObject<Vector, i32>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: Iterator<Item = i32> + ExactSizeIterator> ToR4<RObject<Vector, i32>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_i32(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a u8> + ExactSizeIterator> ToR2<RObject<Vector, u8>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a mut u8> + ExactSizeIterator> ToR3<RObject<Vector, u8>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: Iterator<Item = u8> + ExactSizeIterator> ToR4<RObject<Vector, u8>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_u8(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a bool> + ExactSizeIterator> ToR2<RObject<Vector, bool>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if *from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a mut bool> + ExactSizeIterator> ToR3<RObject<Vector, bool>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if *from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

impl<T: Iterator<Item = bool> + ExactSizeIterator> ToR4<RObject<Vector, bool>> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_bool(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = if from {
                Rboolean_TRUE as i32
            } else {
                Rboolean_FALSE as i32
            };
        }
        result
    }
}

impl From<SEXP> for RObject {
    fn from(x: SEXP) -> Self {
        R::new_object(x)
    }
}

impl From<RObject> for SEXP {
    fn from(x: RObject) -> Self {
        x.sexp
    }
}

impl From<()> for RObject {
    fn from(_: ()) -> Self {
        R::null()
    }
}

impl<RType: Convertible, RMode> From<RObject<RType, RMode>> for RObject {
    fn from(x: RObject<RType, RMode>) -> Self {
        x.convert()
    }
}

impl Deref for RObject {
    type Target = SEXP;
    fn deref(&self) -> &Self::Target {
        &self.sexp
    }
}
