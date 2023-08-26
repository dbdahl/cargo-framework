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

pub struct R {}

#[doc(hidden)]
pub struct AnyType(());

#[doc(hidden)]
pub struct Vector(());

#[doc(hidden)]
pub struct Matrix(());

#[doc(hidden)]
pub struct Array(());

#[doc(hidden)]
pub struct Function(());

#[doc(hidden)]
pub struct ExternalPtr(());

#[doc(hidden)]
pub struct Unknown(());

#[doc(hidden)]
pub struct Character;

#[doc(hidden)]
pub struct List(());

#[doc(hidden)]
pub struct DataFrame(());

pub trait HasLength {}
impl HasLength for Vector {}
impl HasLength for Matrix {}
impl HasLength for Array {}

pub trait Atomic {}
impl Atomic for f64 {}
impl Atomic for i32 {}
impl Atomic for u8 {}
impl Atomic for bool {}
impl Atomic for Character {}
impl Atomic for Unknown {}

#[doc(hidden)]
pub trait Convertible {}
impl Convertible for Vector {}
impl Convertible for Matrix {}
impl Convertible for Array {}
impl Convertible for Function {}
impl Convertible for ExternalPtr {}

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

    pub fn new_vector_double(length: usize, pc: &mut Pc) -> RObject<Vector, f64> {
        Self::new_vector::<f64>(REALSXP, length, pc)
    }

    pub fn new_vector_integer(length: usize, pc: &mut Pc) -> RObject<Vector, i32> {
        Self::new_vector::<i32>(INTSXP, length, pc)
    }

    pub fn new_vector_raw(length: usize, pc: &mut Pc) -> RObject<Vector, u8> {
        Self::new_vector::<u8>(RAWSXP, length, pc)
    }

    pub fn new_vector_logical(length: usize, pc: &mut Pc) -> RObject<Vector, bool> {
        Self::new_vector::<bool>(LGLSXP, length, pc)
    }

    pub fn new_vector_character(length: usize, pc: &mut Pc) -> RObject<Vector, Character> {
        Self::new_vector(STRSXP, length, pc)
    }

    fn new_matrix<T>(code: u32, nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, T> {
        Self::wrap(pc.protect(unsafe {
            Rf_allocMatrix(code, nrow.try_into().unwrap(), ncol.try_into().unwrap())
        }))
    }

    pub fn new_matrix_double(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, f64> {
        Self::new_matrix::<f64>(REALSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_integer(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, i32> {
        Self::new_matrix::<i32>(INTSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_raw(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, u8> {
        Self::new_matrix::<u8>(RAWSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_logical(nrow: usize, ncol: usize, pc: &mut Pc) -> RObject<Matrix, bool> {
        Self::new_matrix::<bool>(LGLSXP, nrow, ncol, pc)
    }

    pub fn new_matrix_character(
        nrow: usize,
        ncol: usize,
        pc: &mut Pc,
    ) -> RObject<Matrix, Character> {
        Self::new_matrix::<Character>(STRSXP, nrow, ncol, pc)
    }

    fn new_array<T>(code: u32, dim: &[usize], pc: &mut Pc) -> RObject<Array, T> {
        let d = dim.iter().map(|x| i32::try_from(*x).unwrap()).to_r(pc);
        Self::wrap(pc.protect(unsafe { Rf_allocArray(code, d.sexp) }))
    }

    pub fn new_array_double(dim: &[usize], pc: &mut Pc) -> RObject<Array, f64> {
        Self::new_array::<f64>(REALSXP, dim, pc)
    }

    pub fn new_array_integer(dim: &[usize], pc: &mut Pc) -> RObject<Array, i32> {
        Self::new_array::<i32>(INTSXP, dim, pc)
    }

    pub fn new_array_raw(dim: &[usize], pc: &mut Pc) -> RObject<Array, u8> {
        Self::new_array::<u8>(RAWSXP, dim, pc)
    }

    pub fn new_array_logical(dim: &[usize], pc: &mut Pc) -> RObject<Array, bool> {
        Self::new_array::<bool>(LGLSXP, dim, pc)
    }

    pub fn new_array_character(dim: &[usize], pc: &mut Pc) -> RObject<Array, Character> {
        Self::new_array::<Character>(STRSXP, dim, pc)
    }

    pub fn new_list(length: usize, pc: &mut Pc) -> RObject<Vector, List> {
        Self::new_vector(VECSXP, length, pc)
    }

    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new_error(message: &str, pc: &mut Pc) -> RObject {
        let list = Self::new_list(2, pc);
        let _ = list.set(0, &message.to_r(pc));
        let _ = list.set(1, &Self::null());
        let _ = list.set_names(&["message", "calls"].to_r(pc));
        list.set_class(&["error", "condition"].to_r(pc));
        list.convert()
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

    pub fn na_double() -> f64 {
        unsafe { R_NaReal }
    }

    pub fn na_integer() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn na_logical() -> i32 {
        unsafe { R_NaInt }
    }

    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    pub fn infinity_positive() -> f64 {
        unsafe { R_PosInf }
    }

    pub fn infinity_negative() -> f64 {
        unsafe { R_NegInf }
    }

    pub fn is_na_double(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    pub fn is_na_integer(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    pub fn is_na_logical(x: i32) -> bool {
        x == unsafe { R_NaInt }
    }

    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
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
pub struct RObject<RType = AnyType, RMode = Unknown> {
    pub sexp: SEXP,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {
    fn convert<RTypeTo, RModeTo>(&self) -> RObject<RTypeTo, RModeTo> {
        R::wrap(self.sexp)
    }

    pub fn as_unknown(&self) -> RObject {
        self.convert()
    }

    pub fn as_vector(&self) -> Result<RObject<Vector, Unknown>, &'static str> {
        if unsafe { Rf_isVectorAtomic(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not an atomic vector")
        }
    }

    pub fn as_matrix(&self) -> Result<RObject<Matrix, Unknown>, &'static str> {
        if unsafe { Rf_isMatrix(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not an atomic vector")
        }
    }

    pub fn as_array(&self) -> Result<RObject<Array, Unknown>, &'static str> {
        if unsafe { Rf_isArray(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not an atomic vector")
        }
    }

    pub fn as_list(&self) -> Result<RObject<Vector, List>, &'static str> {
        if unsafe { Rf_isVectorList(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not a vector list")
        }
    }

    pub fn as_data_frame(&self) -> Result<RObject<Vector, DataFrame>, &'static str> {
        if unsafe { Rf_isFrame(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function(&self) -> Result<RObject<Function, Unknown>, &'static str> {
        if unsafe { Rf_isFunction(self.sexp) != 0 } {
            Ok(self.convert())
        } else {
            Err("Not a function")
        }
    }

    pub fn as_external_ptr(&self) -> Result<RObject<ExternalPtr, Unknown>, &'static str> {
        if unsafe { TYPEOF(self.sexp) == EXTPTRSXP as i32 } {
            Ok(self.convert())
        } else {
            Err("Not an external pointer")
        }
    }

    pub fn as_f64(&self) -> Result<f64, &'static str> {
        let msg = "Cannot be interpreted as an f64";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    Ok(unsafe { Rf_asReal(self.sexp) })
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn as_i32(&self) -> Result<i32, &'static str> {
        let msg = "Cannot be interpreted as an i32";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_integer() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        if x == i32::MIN {
                            Err("Value equals NA in R")
                        } else {
                            Ok(x)
                        }
                    } else if s.is_mode_double() {
                        let y = unsafe { Rf_asReal(s.sexp) };
                        if y > f64::from(i32::MAX) {
                            Err("Value greater than maximum i32")
                        } else if y == f64::from(i32::MIN) {
                            Err("Value equals NA in R")
                        } else if y < f64::from(i32::MIN) {
                            Err("Value less than minumum i32")
                        } else if y.is_nan() {
                            Err("Value equal NaN in R")
                        } else {
                            Ok(y.round() as i32)
                        }
                    } else if s.is_mode_raw() {
                        Ok(unsafe { Rf_asInteger(s.sexp) })
                    } else if s.is_mode_logical() {
                        let y = unsafe { Rf_asLogical(s.sexp) };
                        if y == i32::MIN {
                            Err("Value equals R's NA for bool")
                        } else {
                            Ok(y)
                        }
                    } else {
                        Err(msg)
                    }
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn as_usize(&self) -> Result<usize, &'static str> {
        let msg = "Cannot be interpreted as an usize";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_integer() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        usize::try_from(x).map_err(|_| msg)
                    } else if s.is_mode_double() {
                        let y = unsafe { Rf_asReal(s.sexp) };
                        let z = y as usize;
                        if z as f64 == y {
                            Ok(z)
                        } else {
                            Err(msg)
                        }
                    } else if s.is_mode_raw() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        usize::try_from(x).map_err(|_| msg)
                    } else if s.is_mode_logical() {
                        let x = unsafe { Rf_asLogical(s.sexp) };
                        if x == i32::MIN {
                            Err("Value equals R's NA for bool")
                        } else {
                            usize::try_from(x).map_err(|_| msg)
                        }
                    } else {
                        Err("Cannot be interpreted as an i32")
                    }
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn as_u8(&self) -> Result<u8, &'static str> {
        let msg = "Cannot be interpreted as an u8";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_integer() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        u8::try_from(x).map_err(|_| msg)
                    } else if s.is_mode_double() {
                        let y = unsafe { Rf_asReal(s.sexp) };
                        let z = y as u8;
                        if z as f64 == y {
                            Ok(z)
                        } else {
                            Err(msg)
                        }
                    } else if s.is_mode_raw() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        u8::try_from(x).map_err(|_| msg)
                    } else if s.is_mode_logical() {
                        let x = unsafe { Rf_asLogical(s.sexp) };
                        if x == i32::MIN {
                            Err("Value equals R's NA for bool")
                        } else {
                            u8::try_from(x).map_err(|_| msg)
                        }
                    } else {
                        Err(msg)
                    }
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn as_bool(&self) -> Result<bool, &'static str> {
        let msg = "Cannot be interpreted as a bool";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_integer() {
                        let x = unsafe { Rf_asInteger(s.sexp) };
                        if x == i32::MIN {
                            Err("Value equals NA in R")
                        } else {
                            Ok(x != 0)
                        }
                    } else if s.is_mode_double() {
                        let y = unsafe { Rf_asReal(s.sexp) };
                        if R::is_na_double(y) || R::is_nan(y) {
                            Err(msg)
                        } else {
                            Ok(y != 0.0)
                        }
                    } else if s.is_mode_raw() {
                        Ok(unsafe { Rf_asInteger(s.sexp) } != 0)
                    } else if s.is_mode_logical() {
                        let y = unsafe { Rf_asLogical(s.sexp) };
                        if y == i32::MIN {
                            Err("Value equals R's NA for bool")
                        } else {
                            Ok(y != 0)
                        }
                    } else {
                        Err(msg)
                    }
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn as_str(&self) -> Result<&str, &'static str> {
        let msg = "Cannot be interpreted as an &str";
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    let mut pc = Pc::new();
                    s.to_mode_character(&mut pc).get(0)
                } else {
                    Err(msg)
                }
            }
            Err(_) => Err(msg),
        }
    }

    pub fn is_null(&self) -> bool {
        unsafe { Rf_isNull(self.sexp) != 0 }
    }

    pub fn is_na(&self) -> bool {
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_double() {
                        unsafe { R_IsNA(Rf_asReal(s.sexp)) != 0 }
                    } else if s.is_mode_integer() {
                        unsafe { Rf_asInteger(s.sexp) == R::na_integer() }
                    } else if s.is_mode_logical() {
                        unsafe { Rf_asLogical(s.sexp) == R::na_logical() }
                    } else if s.is_mode_character() {
                        unsafe { Rf_asChar(s.sexp) == R_NaString }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }

    pub fn is_nan(&self) -> bool {
        match self.as_vector() {
            Ok(s) => {
                if s.is_scalar() {
                    if s.is_mode_double() {
                        unsafe { R_IsNaN(Rf_asReal(s.sexp)) != 0 }
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            Err(_) => false,
        }
    }

    pub fn get_class(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_ClassSymbol) })
    }

    pub fn set_class(&self, names: &RObject<Vector, Character>) {
        unsafe {
            Rf_classgets(self.sexp, names.sexp);
        }
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

    /// Duplicate an object.
    ///
    /// Multiple symbols may be bound to the same object, so if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy first.
    /// E.g, call this method on arguments pass via `.Call` before modifying them.
    ///
    pub fn duplicate(&self, pc: &mut Pc) -> RObject<RType, RMode> {
        R::wrap(pc.protect(unsafe { Rf_duplicate(self.sexp) }))
    }
}

impl<S: HasLength, T> RObject<S, T> {
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.sexp) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    pub fn is_empty(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 0 }
    }

    pub fn is_scalar(&self) -> bool {
        unsafe { Rf_xlength(self.sexp) == 1 }
    }
}

impl<S: HasLength, T: Atomic> RObject<S, T> {
    fn slice_engine<U>(&self, data: *mut U) -> &'static mut [U] {
        let len = self.len();
        unsafe { std::slice::from_raw_parts_mut(data, len) }
    }

    pub fn is_mode_double(&self) -> bool {
        unsafe { Rf_isReal(self.sexp) != 0 }
    }

    pub fn is_mode_integer(&self) -> bool {
        unsafe { Rf_isInteger(self.sexp) != 0 }
    }

    pub fn is_mode_raw(&self) -> bool {
        unsafe { TYPEOF(self.sexp) == RAWSXP as i32 }
    }

    pub fn is_mode_logical(&self) -> bool {
        unsafe { Rf_isLogical(self.sexp) != 0 }
    }

    pub fn is_mode_character(&self) -> bool {
        unsafe { Rf_isString(self.sexp) != 0 }
    }

    pub fn as_mode_double(&self) -> Result<RObject<S, f64>, &'static str> {
        if self.is_mode_double() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn as_mode_integer(&self) -> Result<RObject<S, i32>, &'static str> {
        if self.is_mode_integer() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn as_mode_raw(&self) -> Result<RObject<S, u8>, &'static str> {
        if self.is_mode_raw() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn as_mode_logical(&self) -> Result<RObject<S, bool>, &'static str> {
        if self.is_mode_logical() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn as_mode_character(&self) -> Result<RObject<S, Character>, &'static str> {
        if self.is_mode_character() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn to_mode_double(&self, pc: &mut Pc) -> RObject<S, f64> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, REALSXP) }))
    }

    pub fn to_mode_integer(&self, pc: &mut Pc) -> RObject<S, i32> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, INTSXP) }))
    }

    pub fn to_mode_raw(&self, pc: &mut Pc) -> RObject<S, u8> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, RAWSXP) }))
    }

    pub fn to_mode_logical(&self, pc: &mut Pc) -> RObject<S, bool> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, LGLSXP) }))
    }

    pub fn to_mode_character(&self, pc: &mut Pc) -> RObject<S, Character> {
        R::wrap(pc.protect(unsafe { Rf_coerceVector(self.sexp, STRSXP) }))
    }
}

impl<S: HasLength> RObject<S, f64> {
    pub fn slice(&self) -> &'static mut [f64] {
        self.slice_engine(unsafe { REAL(self.sexp) })
    }
}

impl<S: HasLength> RObject<S, i32> {
    pub fn slice(&self) -> &'static mut [i32] {
        self.slice_engine(unsafe { INTEGER(self.sexp) })
    }
}

impl<S: HasLength> RObject<S, u8> {
    pub fn slice(&self) -> &'static mut [u8] {
        self.slice_engine(unsafe { RAW(self.sexp) })
    }
}

impl<S: HasLength> RObject<S, bool> {
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

    // Manipulates the matrix in place to be a vector by dropping the `dim` attribute.
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

impl RObject<Function, Unknown> {
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
    fn get_engine<T>(
        &self,
        index: usize,
        f: unsafe extern "C" fn(SEXP, isize) -> T,
    ) -> Result<T, &'static str> {
        if index < self.len() {
            Ok(unsafe { f(self.sexp, index.try_into().unwrap()) })
        } else {
            Err("Index out of bounds")
        }
    }

    fn set_engine<T>(
        &self,
        index: usize,
        value: T,
        f: unsafe extern "C" fn(SEXP, isize, T),
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { f(self.sexp, index.try_into().unwrap(), value) }
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }

    pub fn get_names(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_NamesSymbol) })
    }

    pub fn set_names(&self, names: &RObject<Vector, Character>) -> Result<(), &'static str> {
        if unsafe { Rf_length(names.sexp) != Rf_length(self.sexp) } {
            return Err("Length of names is not correct");
        }
        unsafe {
            Rf_namesgets(self.sexp, names.sexp);
        }
        Ok(())
    }
}

impl RObject<Vector, f64> {
    pub fn get(&self, index: usize) -> Result<f64, &'static str> {
        self.get_engine(index, REAL_ELT)
    }

    pub fn set(&self, index: usize, value: f64) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_REAL_ELT)
    }
}

impl RObject<Vector, i32> {
    pub fn get(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, INTEGER_ELT)
    }

    pub fn set(&self, index: usize, value: i32) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_INTEGER_ELT)
    }
}

impl RObject<Vector, u8> {
    pub fn get(&self, index: usize) -> Result<u8, &'static str> {
        self.get_engine(index, RAW_ELT)
    }

    pub fn set(&self, index: usize, value: u8) -> Result<(), &'static str> {
        self.set_engine(index, value, SET_RAW_ELT)
    }
}

impl RObject<Vector, bool> {
    pub fn get(&self, index: usize) -> Result<bool, &'static str> {
        self.get_engine(index, LOGICAL_ELT).map(|x| x != 0)
    }

    pub fn get_i32(&self, index: usize) -> Result<i32, &'static str> {
        self.get_engine(index, LOGICAL_ELT)
    }

    pub fn set(&self, index: usize, value: bool) -> Result<(), &'static str> {
        let value = if value {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }

    pub fn set_i32(&self, index: usize, value: i32) -> Result<(), &'static str> {
        let value = if value != 0 {
            Rboolean_TRUE as i32
        } else {
            Rboolean_FALSE as i32
        };
        self.set_engine(index, value, SET_LOGICAL_ELT)
    }
}

impl RObject<Vector, Character> {
    pub fn get<'a>(&self, index: usize) -> Result<&'a str, &'static str> {
        match self.get_engine(index, STRING_ELT) {
            Ok(sexp) => {
                let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(sexp)) as *const c_char) };
                c_str.to_str().map_err(|_| "Not valid UTF8")
            }
            Err(e) => Err(e),
        }
    }

    pub fn set(&self, index: usize, value: &str) -> Result<(), &'static str> {
        unsafe {
            let value = Rf_mkCharLenCE(
                value.as_ptr() as *const c_char,
                value.len().try_into().unwrap(),
                cetype_t_CE_UTF8,
            );
            self.set_engine(index, value, SET_STRING_ELT)
        }
    }

    pub fn set_na(&self, index: usize) {
        unsafe {
            SET_STRING_ELT(self.sexp, index.try_into().unwrap(), R_NaString);
        }
    }
}

impl RObject<Vector, List> {
    pub fn get(&self, index: usize) -> Result<RObject, &'static str> {
        self.get_engine(index, VECTOR_ELT).map(R::wrap)
    }

    pub fn set<RType, RMode>(
        &self,
        index: usize,
        value: &RObject<RType, RMode>,
    ) -> Result<(), &'static str> {
        if index < self.len() {
            unsafe { SET_VECTOR_ELT(self.sexp, index.try_into().unwrap(), value.sexp) };
            Ok(())
        } else {
            Err("Index out of bounds")
        }
    }

    pub fn to_data_frame(
        &self,
        names: &RObject<Vector, Character>,
        row_names: &RObject<Vector, Character>,
        pc: &mut Pc,
    ) -> Result<RObject<Vector, DataFrame>, &'static str> {
        if names.len() != self.len() {
            return Err("Length of names is not correct");
        }
        let mut nrow = -1;
        for i in 0..self.len() {
            let x = self.get(i).unwrap();
            if unsafe { Rf_isVectorAtomic(x.sexp) == 0 } {
                return Err("Expected an atomic vector... Have you set the list elements yet?");
            }
            let len = unsafe { Rf_xlength(x.sexp) };
            if i == 0 {
                nrow = len;
            } else if len != nrow {
                return Err("Inconsistent number of rows among list elements");
            }
        }
        if row_names.len() != nrow as usize {
            return Err("Length of row names is not correct");
        }
        self.set_names(names)?;
        unsafe { Rf_setAttrib(self.sexp, R_RowNamesSymbol, row_names.sexp) };
        self.set_class(&["data.frame"].to_r(pc));
        Ok(self.convert())
    }
}

impl RObject<Vector, DataFrame> {
    pub fn get(&self, index: usize) -> Result<RObject, &'static str> {
        self.convert::<Vector, List>().get(index)
    }

    pub fn set<RType, RMode>(
        &self,
        index: usize,
        value: &RObject<RType, RMode>,
    ) -> Result<(), &'static str> {
        self.convert::<Vector, List>().set(index, value)
    }

    pub fn get_row_names(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_RowNamesSymbol) })
    }

    pub fn set_col_names(
        &self,
        row_names: &RObject<Vector, Character>,
    ) -> Result<(), &'static str> {
        if unsafe { Rf_length(row_names.sexp) != Rf_length(self.sexp) } {
            return Err("Length of row names is not correct");
        }
        unsafe { Rf_setAttrib(self.sexp, R_RowNamesSymbol, row_names.sexp) };
        Ok(())
    }
}

impl<RMode> RObject<Matrix, RMode> {
    pub fn index(&self, (i, j): (usize, usize)) -> usize {
        let nrow = self.nrow();
        nrow * j + i
    }

    pub fn get_dimnames(&self) -> RObject<Vector, Character> {
        R::wrap(unsafe { Rf_getAttrib(self.sexp, R_DimNamesSymbol) })
    }

    pub fn set_dimnames(&self, dimnames: &RObject<Vector, List>) -> Result<(), &'static str> {
        if dimnames.len() != 2 {
            return Err("Length should be two");
        }
        let rownames = dimnames.get(0).unwrap();
        if rownames.as_vector().map(|x| x.is_mode_character()) != Ok(true) {
            return Err("Row names must be a character vector");
        }
        let rownames: RObject<Vector, Character> = rownames.convert();
        if rownames.len() != self.nrow() {
            return Err("Row names do not match the number of rows");
        }
        let colnames = dimnames.get(1).unwrap();
        if colnames.as_vector().map(|x| x.is_mode_character()) != Ok(true) {
            return Err("Column names must be a character vector");
        }
        let colnames: RObject<Vector, Character> = colnames.convert();
        if colnames.len() != self.ncol() {
            return Err("Column names do not match the number of columns");
        }
        unsafe {
            Rf_dimnamesgets(self.sexp, dimnames.sexp);
        }
        Ok(())
    }
}

impl RObject<Matrix, f64> {
    pub fn get(&self, index: (usize, usize)) -> Result<f64, &'static str> {
        self.convert::<Vector, f64>().get(self.index(index))
    }

    pub fn set(&self, index: (usize, usize), value: f64) -> Result<(), &'static str> {
        self.convert::<Vector, f64>().set(self.index(index), value)
    }
}

impl RObject<Matrix, i32> {
    pub fn get(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.convert::<Vector, i32>().get(self.index(index))
    }

    pub fn set(&self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        self.convert::<Vector, i32>().set(self.index(index), value)
    }
}

impl RObject<Matrix, u8> {
    pub fn get(&self, index: (usize, usize)) -> Result<u8, &'static str> {
        self.convert::<Vector, u8>().get(self.index(index))
    }

    pub fn set(&self, index: (usize, usize), value: u8) -> Result<(), &'static str> {
        self.convert::<Vector, u8>().set(self.index(index), value)
    }
}

impl RObject<Matrix, bool> {
    pub fn get(&self, index: (usize, usize)) -> Result<bool, &'static str> {
        self.convert::<Vector, bool>().get(self.index(index))
    }

    pub fn get_i32(&self, index: (usize, usize)) -> Result<i32, &'static str> {
        self.convert::<Vector, bool>().get_i32(self.index(index))
    }

    pub fn set(&self, index: (usize, usize), value: bool) -> Result<(), &'static str> {
        self.convert::<Vector, bool>().set(self.index(index), value)
    }

    pub fn set_i32(&self, index: (usize, usize), value: i32) -> Result<(), &'static str> {
        self.convert::<Vector, bool>()
            .set_i32(self.index(index), value)
    }
}

impl RObject<Matrix, Character> {
    pub fn get(&self, index: (usize, usize)) -> Result<&str, &'static str> {
        self.convert::<Vector, Character>().get(self.index(index))
    }

    pub fn set<RType, RMode>(
        &self,
        index: (usize, usize),
        value: &str,
    ) -> Result<(), &'static str> {
        self.convert::<Vector, Character>()
            .set(self.index(index), value)
    }
}

impl<T> RObject<ExternalPtr, T> {
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
    pub fn decode_as_mut_ref(&mut self) -> &mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.sexp) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    pub fn set_type<S>(&self) -> RObject<ExternalPtr, S> {
        self.convert()
    }

    /// Get tag for an R external pointer
    ///
    /// This method get the tag associated with an R external pointer, which was set by [`Self::external_pointer_encode`].
    ///
    pub fn tag(&self) -> RObject {
        R::wrap(unsafe { R_ExternalPtrTag(self.sexp) })
    }
}

// Conversions

pub trait ToR1<S, T> {
    fn to_r(&self, pc: &mut Pc) -> RObject<S, T>;
}

pub trait ToR2<S, T> {
    fn to_r(self, pc: &mut Pc) -> RObject<S, T>;
}

pub trait ToR3<S, T> {
    fn to_r(self, pc: &mut Pc) -> RObject<S, T>;
}

pub trait ToR4<S, T> {
    fn to_r(self, pc: &mut Pc) -> RObject<S, T>;
}

// f64

impl ToR1<Vector, f64> for f64 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        R::wrap(pc.protect(unsafe { Rf_ScalarReal(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, f64> for [f64; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, f64> for &[f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_double(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, f64> for &mut [f64] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_double(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a f64> + ExactSizeIterator> ToR2<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_double(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: IntoIterator<Item = &'a mut f64> + ExactSizeIterator> ToR3<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_double(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: IntoIterator<Item = f64> + ExactSizeIterator> ToR4<Vector, f64> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, f64> {
        let result = R::new_vector_double(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// i32

impl ToR1<Vector, i32> for i32 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, i32> for [i32; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, i32> for &[i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, i32> for &mut [i32] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: Iterator<Item = &'a i32> + ExactSizeIterator> ToR2<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a mut i32> + ExactSizeIterator> ToR3<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: Iterator<Item = i32> + ExactSizeIterator> ToR4<Vector, i32> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// usize

impl ToR1<Vector, i32> for usize {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        R::wrap(pc.protect(unsafe { Rf_ScalarInteger((*self).try_into().unwrap()) }))
    }
}

impl<const N: usize> ToR1<Vector, i32> for [usize; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, i32> for &[usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl ToR1<Vector, i32> for &mut [usize] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, i32> {
        let result = R::new_vector_integer(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

// u8

impl ToR1<Vector, u8> for u8 {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        R::wrap(pc.protect(unsafe { Rf_ScalarRaw(*self) }))
    }
}

impl<const N: usize> ToR1<Vector, u8> for [u8; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, u8> for &[u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl ToR1<Vector, u8> for &mut [u8] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice();
        slice.copy_from_slice(self);
        result
    }
}

impl<'a, T: Iterator<Item = &'a u8> + ExactSizeIterator> ToR2<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a mut u8> + ExactSizeIterator> ToR3<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = *from;
        }
        result
    }
}

impl<T: Iterator<Item = u8> + ExactSizeIterator> ToR4<Vector, u8> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, u8> {
        let result = R::new_vector_raw(self.len(), pc);
        let slice = result.slice();
        for (to, from) in slice.iter_mut().zip(self) {
            *to = from;
        }
        result
    }
}

// bool

impl ToR1<Vector, bool> for bool {
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

impl<const N: usize> ToR1<Vector, bool> for [bool; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, bool> for &[bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_logical(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl ToR1<Vector, bool> for &mut [bool] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_logical(self.len(), pc);
        let slice = result.slice();
        for (i, j) in slice.iter_mut().zip(self.iter()) {
            *i = (*j).try_into().unwrap();
        }
        result
    }
}

impl<'a, T: Iterator<Item = &'a bool> + ExactSizeIterator> ToR2<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_logical(self.len(), pc);
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

impl<'a, T: Iterator<Item = &'a mut bool> + ExactSizeIterator> ToR3<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_logical(self.len(), pc);
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

impl<T: Iterator<Item = bool> + ExactSizeIterator> ToR4<Vector, bool> for T {
    fn to_r(self, pc: &mut Pc) -> RObject<Vector, bool> {
        let result = R::new_vector_logical(self.len(), pc);
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

// &str

impl ToR1<Vector, Character> for &str {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
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

impl<const N: usize> ToR1<Vector, Character> for [&str; N] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        self.as_ref().to_r(pc)
    }
}

impl ToR1<Vector, Character> for &[&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        let result = R::new_vector_character(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

impl ToR1<Vector, Character> for &mut [&str] {
    fn to_r(&self, pc: &mut Pc) -> RObject<Vector, Character> {
        let result = R::new_vector_character(self.len(), pc);
        for (index, s) in self.iter().enumerate() {
            let _ = result.set(index, s);
        }
        result
    }
}

// RObject and SEXP

impl<RType, RMode> ToR1<RType, RMode> for RObject<RType, RMode> {
    fn to_r(&self, _pc: &mut Pc) -> RObject<RType, RMode> {
        self.convert()
    }
}

impl ToR1<AnyType, Unknown> for SEXP {
    fn to_r(&self, _pc: &mut Pc) -> RObject<AnyType, Unknown> {
        R::wrap(*self)
    }
}

impl ToR1<AnyType, Unknown> for () {
    fn to_r(&self, _pc: &mut Pc) -> RObject<AnyType, Unknown> {
        R::null()
    }
}

// Deref

impl Deref for RObject {
    type Target = SEXP;
    fn deref(&self) -> &Self::Target {
        &self.sexp
    }
}
