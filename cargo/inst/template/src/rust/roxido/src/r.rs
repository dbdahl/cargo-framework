//! Extension Framework for R using Rust

#![allow(dead_code)]

// Helpful resources:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals
//   https://www.tidyverse.org/blog/2019/05/resource-cleanup-in-c-and-the-r-api
//   https://github.com/wch/r-source

use crate::rbindings::*;
use std::convert::{TryFrom, TryInto};
use std::ffi::CStr;
use std::num::TryFromIntError;
use std::ops::Deref;
use std::os::raw::{c_char, c_void};
use std::str::Utf8Error;

pub struct R;

impl R {
    /// Generate random bytes using R's RNG.
    ///
    /// # Examples:
    /// ```
    /// let mut rng = rand_pcg::Pcg64Mcg::from_seed(crate::r::random_bytes::<16>());
    /// ```
    ///
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

    /// Get R's definition of the `Inf` value.
    pub fn infinity_positive() -> f64 {
        unsafe { R_PosInf }
    }

    /// Get R's definition of the `-Inf` value.
    pub fn infinity_negative() -> f64 {
        unsafe { R_NegInf }
    }

    /// Get R's definition of the `NaN` value.
    pub fn nan() -> f64 {
        unsafe { R_NaN }
    }

    /// Get R's definition of the `NA` value of storage mode `double`.
    pub fn na_double() -> f64 {
        unsafe { R_NaReal }
    }

    /// Get R's definition of the `NA` value of storage mode `integer`.
    pub fn na_integer() -> i32 {
        unsafe { R_NaInt }
    }

    /// Get R's definition of the `NA` value of storage mode `logical`.
    pub fn na_logical() -> i32 {
        unsafe { R_NaInt }
    }

    /// Test if value is finite.
    pub fn is_finite(x: f64) -> bool {
        unsafe { R_finite(x) != 0 }
    }

    /// Test if value is `NaN`.
    pub fn is_nan(x: f64) -> bool {
        unsafe { R_IsNaN(x) != 0 }
    }

    /// Test if value is NA for storage mode `double`.
    pub fn is_na_double(x: f64) -> bool {
        unsafe { R_IsNA(x) != 0 }
    }

    /// Test if value is NA for storage mode `integer`.
    pub fn is_na_integer(x: i32) -> bool {
        unsafe { x == R_NaInt }
    }

    /// Test if value is NA for storage mode `logical`.
    pub fn is_na_logical(x: i32) -> bool {
        unsafe { x == R_NaInt }
    }
}

/// Print to the R console.
///
/// This is an implementation detail and *should not* be called directly!
/// This returns `true` if the print statement swallowed a user interrupt.
/// R checks for user interrupt every 100 print statements.
/// See the `Rvprintf` function in `printutils.c` of R's source.
///
#[doc(hidden)]
pub fn _print(x: &str, use_stdout: bool) -> bool {
    #[repr(C)]
    struct DummyFat {
        len: usize,
        ptr: *const c_char,
        use_stdout: bool,
    }
    let mut y = DummyFat {
        len: x.len(),
        ptr: x.as_ptr() as *const c_char,
        use_stdout,
    };
    let y_ptr = &mut y as *mut DummyFat as *mut c_void;
    extern "C" fn print_fn(y_ptr: *mut c_void) {
        unsafe {
            let y_ptr = y_ptr as *mut DummyFat;
            if (*y_ptr).use_stdout {
                Rprintf(
                    b"%.*s\0".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            } else {
                REprintf(
                    b"%.*s\0".as_ptr() as *const c_char,
                    (*y_ptr).len,
                    (*y_ptr).ptr,
                );
            }
        }
    }
    unsafe { R_ToplevelExec(Some(print_fn), y_ptr) == 0 }
}

/// Just like Rust's usual `print!` macro, except output goes to the R console.
#[macro_export]
macro_rules! rprint {
    ($fmt_string:expr) => {
        r::_print(format!($fmt_string).as_str(), true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        r::_print(format!($fmt_string, $($arg),*).as_str(), true)
    }
}

/// Just like Rust's usual `println!` macro, except output goes to the R console.
#[macro_export]
macro_rules! rprintln {
    () => {
        r::_print("\n", true)
    };
    ($fmt_string:expr) => {
        r::_print(format!(concat!($fmt_string,"\n")).as_str(), true)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        r::_print(format!(concat!($fmt_string,"\n"), $($arg),*).as_str(), true)
    }
}

/// Just like Rust's usual `eprint!` macro, except output goes to the R console.
#[macro_export]
macro_rules! reprint {
    ($fmt_string:expr) => {
        r::_print(format!($fmt_string).as_str(), false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        r::_print(format!($fmt_string, $($arg),*).as_str(), false)
    }
}

/// Just like Rust's usual `eprintln!` macro, except output goes to the R console.
#[macro_export]
macro_rules! reprintln {
    () => {
        r::_print("\n", false)
    };
    ($fmt_string:expr) => {
        r::_print(format!(concat!($fmt_string,"\n")).as_str(), false)
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        r::_print(format!(concat!($fmt_string,"\n"), $($arg),*).as_str(), false)
    }
}

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error.
#[macro_export]
macro_rules! stop {
    () => {
        std::panic::panic_any(RStopHelper(String::new()))
    };
    ($fmt_string:expr) => {
        std::panic::panic_any(RStopHelper(format!($fmt_string)))
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        std::panic::panic_any(RStopHelper(format!($fmt_string, $($arg),*)))
    }
}

/// A Rust representation of an R object.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RObject(pub SEXP);

/// A Rust representation of R's internal types.
#[repr(u32)]
#[derive(PartialEq, Debug)]
pub enum RObjectType {
    NILSXP = 0,
    SYMSXP = 1,
    LISTSXP = 2,
    CLOSXP = 3,
    ENVSXP = 4,
    PROMSXP = 5,
    LANGSXP = 6,
    SPECIALSXP = 7,
    BUILTINSXP = 8,
    CHARSXP = 9,
    LGLSXP = 10,
    INTSXP = 13,
    REALSXP = 14,
    CPLXSXP = 15,
    STRSXP = 16,
    DOTSXP = 17,
    ANYSXP = 18,
    VECSXP = 19,
    EXPRSXP = 20,
    BCODESXP = 21,
    EXTPTRSXP = 22,
    WEAKREFSXP = 23,
    RAWSXP = 24,
    S4SXP = 25,
    NEWSXP = 30,
    FREESXP = 31,
    FUNSXP = 99,
}

impl RObject {
    /// Map integers for R types to Rust enum.
    pub fn tipe(self) -> RObjectType {
        match unsafe { TYPEOF(self.0) } {
            0 => RObjectType::NILSXP,
            1 => RObjectType::SYMSXP,
            2 => RObjectType::LISTSXP,
            3 => RObjectType::CLOSXP,
            4 => RObjectType::ENVSXP,
            5 => RObjectType::PROMSXP,
            6 => RObjectType::LANGSXP,
            7 => RObjectType::SPECIALSXP,
            8 => RObjectType::BUILTINSXP,
            9 => RObjectType::CHARSXP,
            10 => RObjectType::LGLSXP,
            13 => RObjectType::INTSXP,
            14 => RObjectType::REALSXP,
            15 => RObjectType::CPLXSXP,
            16 => RObjectType::STRSXP,
            17 => RObjectType::DOTSXP,
            18 => RObjectType::ANYSXP,
            19 => RObjectType::VECSXP,
            20 => RObjectType::EXPRSXP,
            21 => RObjectType::BCODESXP,
            22 => RObjectType::EXTPTRSXP,
            23 => RObjectType::WEAKREFSXP,
            24 => RObjectType::RAWSXP,
            25 => RObjectType::S4SXP,
            30 => RObjectType::NEWSXP,
            31 => RObjectType::FREESXP,
            99 => RObjectType::FUNSXP,
            _ => panic!("Unrecognized R type."),
        }
    }

    /// Get the value `NULL`.
    pub fn nil() -> Self {
        Self(unsafe { R_NilValue })
    }

    /// Get R's definition of the `NA` value for an element of an object of storage mode `character`.
    pub fn na_character() -> Self {
        Self(unsafe { R_NaString })
    }

    /// Duplicate an object.
    ///
    /// Since multiple symbols may be bound to the same object, if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy before modifying the copy.
    /// This method is commonly called on arguments to `.Call` before modifying them.
    ///
    pub fn duplicate(self, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_duplicate(self.0) }))
    }

    /// Is the object `NULL`?
    pub fn is_nil(self) -> bool {
        unsafe { Rf_isNull(self.0) != 0 }
    }

    /// Get the length of the object.
    pub fn len(self) -> usize {
        unsafe { Rf_length(self.0).try_into().unwrap() }
    }

    /// Is the length of the object zero?
    pub fn is_empty(self) -> bool {
        unsafe { Rf_length(self.0) == 0 }
    }

    /// Is the object of length one?
    pub fn is_scalar(self) -> bool {
        unsafe { Rf_length(self.0) == 1 }
    }

    /// Is the object of storage mode `double`?
    pub fn is_double(self) -> bool {
        unsafe { Rf_isReal(self.0) != 0 }
    }

    /// Is the object of storage mode `integer`?
    pub fn is_integer(self) -> bool {
        unsafe { Rf_isInteger(self.0) != 0 }
    }

    // Is the object of storage model `double` or `integer` and a scalar?
    pub fn is_double_or_integer_scalar(self) -> bool {
        self.is_scalar() && (self.is_double() || self.is_integer())
    }

    /// Is the object of storage mode `logical`?
    pub fn is_logical(self) -> bool {
        unsafe { Rf_isLogical(self.0) != 0 }
    }

    /// Is the object of storage mode `raw`?
    pub fn is_raw(self) -> bool {
        self.tipe() == RObjectType::RAWSXP
    }

    /// Is the object a symbol?
    pub fn is_symbol(self) -> bool {
        self.tipe() == RObjectType::SYMSXP
    }

    /// Is the object an element of an object of storage mode `character`?
    pub fn is_character_element(self) -> bool {
        self.tipe() == RObjectType::CHARSXP
    }

    /// Is the object of storage mode `character`?
    pub fn is_character(self) -> bool {
        unsafe { Rf_isString(self.0) != 0 }
    }

    /// Test if value is NA for an element of an object of storage mode `character`.
    pub fn is_na_character(self) -> bool {
        unsafe { self.0 == R_NaString }
    }

    /// Can the object be interpreted as `TRUE`?
    pub fn is_true(self) -> bool {
        unsafe { Rf_asLogical(self.0) == Rboolean_TRUE.try_into().unwrap() }
    }

    /// Can the object be interpreted as `FALSE`?
    pub fn is_false(self) -> bool {
        unsafe { Rf_asLogical(self.0) == Rboolean_FALSE.try_into().unwrap() }
    }

    /// Coerce the object to an `f64` value (potentially leading to an `NA`/`NaN` value).
    pub fn as_f64(self) -> f64 {
        unsafe { Rf_asReal(self.0) }
    }

    /// Coerce the object to an `i32` value (potentially leading to an `NA`/`NaN` value).
    pub fn as_i32(self) -> i32 {
        unsafe { Rf_asInteger(self.0) }
    }

    /// Coerce the object to a `bool` value (potentially leading to an `NA`/`NaN` value).
    pub fn as_bool(self) -> bool {
        unsafe { Rf_asLogical(self.0) == Rboolean_TRUE.try_into().unwrap() }
    }

    /// Coerce the object to a `usize` value (potentially leading to an `NA`/`NaN` value), setting negative values to zero.
    pub fn as_usize(self) -> usize {
        let len = unsafe { Rf_asInteger(self.0) };
        len.try_into().unwrap_or(0)
    }

    /// Coerce the object to storage mode `character` and allocate an associated `String` value.
    pub fn as_string(self) -> Result<String, Utf8Error> {
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(self.0)) as *const c_char) };
        c_str.to_str().map(|x| x.to_string())
    }

    /// Coerce the object to storage mode `character` and get the associated `&str` value.
    pub fn as_str(self) -> Result<&'static str, Utf8Error> {
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(self.0)) as *const c_char) };
        c_str.to_str()
    }

    /// Is the object a vector?
    pub fn is_vector(self) -> bool {
        unsafe { Rf_isVector(self.0) != 0 }
    }

    /// Is the object a list?
    ///
    /// Check if the type is LISTSXP or EXPRSXP.
    pub fn is_vector_list(self) -> bool {
        unsafe { Rf_isVectorList(self.0) != 0 }
    }

    /// Is the object an atomic vector?
    ///
    /// Check if type is LGLSXP, INTSXP, REALSXP, CPLXSXP, STRSXP, or RAWSXP.
    pub fn is_vector_atomic(self) -> bool {
        unsafe { Rf_isVectorAtomic(self.0) != 0 }
    }

    /// Treat as an R vector.
    pub fn as_vector(self) -> Result<RVector, &'static str> {
        if !self.is_vector() {
            Err("Not a vector")
        } else {
            Ok(RVector(self))
        }
    }

    /// Treat as an R vector.
    pub fn as_vector_or_stop(self, msg: &str) -> RVector {
        if !self.is_vector() {
            stop!("{}", msg)
        } else {
            RVector(self)
        }
    }

    /// Is the object a list?
    ///
    /// Check if the type is VECSXP.
    pub fn is_list(self) -> bool {
        self.tipe() == RObjectType::VECSXP
    }

    /// Treat as an R list.
    pub fn as_list(self) -> Result<RList, &'static str> {
        if !self.is_list() {
            Err("Not a list")
        } else {
            Ok(RList(RVector(self)))
        }
    }

    /// Treat as an R list.
    pub fn as_list_or_stop(self, msg: &str) -> RList {
        if !self.is_list() {
            stop!("{}", msg)
        } else {
            RList(RVector(self))
        }
    }

    /// Is the object a matrix?
    pub fn is_matrix(self) -> bool {
        unsafe { Rf_isMatrix(self.0) != 0 }
    }

    /// Treat as an R matrix.
    pub fn as_matrix(self) -> Result<RMatrix, &'static str> {
        if !self.is_matrix() {
            Err("Not a matrix")
        } else {
            Ok(RMatrix(RVector(self)))
        }
    }

    /// Treat as an R matrix.
    pub fn as_matrix_or_stop(self, msg: &str) -> RMatrix {
        if !self.is_matrix() {
            stop!("{}", msg)
        } else {
            RMatrix(RVector(self))
        }
    }

    /// Is the object an array?
    pub fn is_array(self) -> bool {
        unsafe { Rf_isArray(self.0) != 0 }
    }

    /// Is the object a data.frame?
    pub fn is_data_frame(self) -> bool {
        unsafe { Rf_isFrame(self.0) != 0 }
    }

    /// Is the object a function?
    pub fn is_function(self) -> bool {
        unsafe { Rf_isFunction(self.0) != 0 }
    }

    /// Treat as an R function.
    pub fn as_function(self) -> Result<RFunction, &'static str> {
        if !self.is_function() {
            Err("Not a function")
        } else {
            Ok(RFunction(self))
        }
    }

    /// Treat as an R function.
    pub fn as_function_or_stop(self, msg: &str) -> RFunction {
        if !self.is_function() {
            stop!("{}", msg);
        } else {
            RFunction(self)
        }
    }

    /// Is the object an environment?
    pub fn is_environment(self) -> bool {
        unsafe { Rf_isEnvironment(self.0) != 0 }
    }

    /// Define a new error.
    ///
    /// This does *not* throw an error.  To throw an R error, simply use `stop!`.
    ///
    pub fn new_error(message: &str, pc: &mut Pc) -> Self {
        let list = RList::new(2, pc);
        let _ = list.set(0, RVectorCharacter::allocate(message, pc));
        let _ = list.set(1, Self::nil());
        let _ = list.names_gets(RVectorCharacter::allocate(["message", "calls"], pc));
        list.class_gets(RVectorCharacter::allocate(["error", "condition"], pc));
        list.into()
    }

    /// Define a new element for a character vector.
    ///
    /// An element of a character vector should generally *not* be returned to a user, but this
    /// function can be used in conjunction with [`RVectorCharacter::set`].
    ///
    pub fn new_character(x: &str, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe {
            Rf_mkCharLen(x.as_ptr() as *const c_char, x.len().try_into().unwrap())
        }))
    }

    /// Define a new symbol.
    pub fn new_symbol(x: &str, pc: &mut Pc) -> Self {
        let sexp = Self::new_character(x, pc).0;
        Self(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    /// Move Rust object to an R external pointer
    ///
    /// This method moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`Self::external_pointer_decode`].
    ///
    pub fn external_pointer_encode<T>(x: T, tag: impl Into<RObject>) -> Self {
        unsafe {
            // Move to Box<_> and then forget about it.
            let ptr = Box::into_raw(Box::new(x)) as *mut c_void;
            Self(R_MakeExternalPtr(ptr, tag.into().0, R_NilValue))
        }
    }

    /// Get tag for an R external pointer
    ///
    /// This method get the tag associated with an R external pointer, which was set by [`Self::external_pointer_encode`].
    ///
    pub fn external_pointer_tag(self) -> Self {
        unsafe { RObject(R_ExternalPtrTag(self.0)) }
    }

    /// Move an R external pointer to a Rust object
    ///
    /// This method moves an R external pointer created by [`Self::external_pointer_encode`] to a Rust object and Rust will then manage its memory.
    ///
    pub fn external_pointer_decode<T>(self) -> T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.0) as *mut T;
            *Box::from_raw(ptr)
        }
    }

    /// Obtain a reference to a Rust object from an R external pointer
    ///
    /// This method obtained a reference to a Rust object from an R external pointer created by [`Self::external_pointer_encode`].
    ///
    pub fn external_pointer_decode_as_ref<T>(self) -> &'static T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.0) as *mut T;
            ptr.as_ref().unwrap()
        }
    }

    /// Obtain a mutable reference to a Rust object from an R external pointer
    ///
    /// This method obtained a mutable reference to a Rust object from an R external pointer created by [`Self::external_pointer_encode`].
    ///
    pub fn external_pointer_decode_as_mut_ref<T>(self) -> &'static mut T {
        unsafe {
            let ptr = R_ExternalPtrAddr(self.0) as *mut T;
            ptr.as_mut().unwrap()
        }
    }

    /// Get an attribute.
    pub fn get_attribute(self, which: &str, pc: &mut Pc) -> Self {
        Self(unsafe { Rf_getAttrib(self.0, Self::new_symbol(which, pc).0) })
    }

    /// Set an attribute.
    pub fn set_attribute(self, which: &str, value: impl Into<RObject>, pc: &mut Pc) {
        unsafe {
            Rf_setAttrib(self.0, Self::new_symbol(which, pc).0, value.into().0);
        }
    }

    /// Create a new binding (or changes the value of an existing binding) in the specified environment frame.  It is the analogue of `assign(self, value, envir = environment, inherits = FALSE)`.
    ///
    /// The function returns an error if the object is not a symbol.
    ///   
    pub fn assign(
        self,
        value: impl Into<RObject>,
        environment: impl Into<RObject>,
    ) -> Result<(), &'static str> {
        if !self.is_symbol() {
            return Err("Not a symbol");
        }
        unsafe { Rf_defineVar(self.0, value.into().0, environment.into().0) };
        Ok(())
    }

    /// Search for an existing binding in the specified environment or its enclosing environments.  If a binding is found, its value is changed to `value`.  Otherwise, a new binding is created in the global environment.  This corresponds to `assign(self, value, envir = environment, inherits = TRUE)`.
    ///
    /// The function returns an error if the object is not a symbol.
    ///   
    pub fn assign_inherits(
        self,
        value: impl Into<RObject>,
        environment: impl Into<RObject>,
    ) -> Result<(), &'static str> {
        if !self.is_symbol() {
            return Err("Not a symbol");
        }
        unsafe { Rf_setVar(self.0, value.into().0, environment.into().0) };
        Ok(())
    }

    /// Set the class attribute of an R object.
    ///
    /// The function returns an error if `value` is not a character vector.
    ///
    pub fn class_gets(self, value: RVectorCharacter) {
        unsafe { Rf_classgets(self.0, value.0 .0 .0) };
    }

    /// Evaluation a expression, returning the value or an error code.
    pub fn eval(self, environment: impl Into<RObject>, pc: &mut Pc) -> Result<RObject, i32> {
        let mut p_out_error: i32 = 0;
        let sexp = pc.protect(unsafe {
            R_tryEval(self.0, environment.into().0, &mut p_out_error as *mut i32)
        });
        match p_out_error {
            0 => Ok(RObject(sexp)),
            e => Err(e),
        }
    }
}

impl Deref for RObject {
    type Target = SEXP;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Vector

/// A Rust representation of an R vector.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RVector(RObject);

impl RVector {
    fn new_vector<T>(
        len: usize,
        code: RObjectType,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> (RVector, &'static mut [T]) {
        unsafe {
            let sexp = pc.protect(Rf_allocVector(code as u32, len.try_into().unwrap()));
            let slice = std::slice::from_raw_parts_mut(get_ptr(sexp), len);
            (Self(RObject(sexp)), slice)
        }
    }

    /// Allocate a new vector with storage mode `double`.
    ///
    /// The values are uninitialized.  Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_double(len: usize, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        Self::new_vector(len, RObjectType::REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Allocate a new vector with storage mode `integer`.
    ///
    /// The values are uninitialized.  Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_integer(len: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_vector(len, RObjectType::INTSXP, |x| unsafe { INTEGER(x) }, pc)
    }

    /// Allocate a new vector with storage mode `logical`.
    ///
    /// The values are uninitialized.  Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_logical(len: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_vector(len, RObjectType::LGLSXP, |x| unsafe { LOGICAL(x) }, pc)
    }

    /// Allocate a new vector with storage mode `raw`.
    ///
    /// The values are uninitialized.  Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_raw(len: usize, pc: &mut Pc) -> (Self, &'static mut [u8]) {
        Self::new_vector(len, RObjectType::RAWSXP, |x| unsafe { RAW(x) }, pc)
    }

    fn slice<T>(
        robject: Self,
        code: RObjectType,
        get_ptr: impl Fn(SEXP) -> *mut T,
    ) -> Result<&'static [T], &'static str> {
        if robject.tipe() == code {
            let len = robject.len();
            let slice = unsafe { std::slice::from_raw_parts(get_ptr(robject.0 .0), len) };
            Ok(slice)
        } else {
            Err("Object is not of the asserted type")
        }
    }

    /// Get an `f64` slice associated with the object, or an error if the object is not of storage mode `double`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_double(self) -> Result<&'static [f64], &'static str> {
        Self::slice(self, RObjectType::REALSXP, |x| unsafe { REAL(x) })
    }

    /// Get an `i32` slice associated with the object, or an error if the object is not of storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_integer(self) -> Result<&'static [i32], &'static str> {
        Self::slice(self, RObjectType::INTSXP, |x| unsafe { INTEGER(x) })
    }

    /// Get an `i32` slice (of logical values) associated with the object, or an error if the object is not of storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_logical(self) -> Result<&'static [i32], &'static str> {
        Self::slice(self, RObjectType::LGLSXP, |x| unsafe { LOGICAL(x) })
    }

    /// Get an `u8` slice associated with the object, or an error if the object is not of storage mode `raw`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_raw(self) -> Result<&'static [u8], &'static str> {
        Self::slice(self, RObjectType::RAWSXP, |x| unsafe { RAW(x) })
    }

    fn slice_mut<T>(
        robject: Self,
        code: RObjectType,
        get_ptr: impl Fn(SEXP) -> *mut T,
    ) -> Result<&'static mut [T], &'static str> {
        if robject.tipe() == code {
            let len = robject.len();
            let slice = unsafe { std::slice::from_raw_parts_mut(get_ptr(robject.0 .0), len) };
            Ok(slice)
        } else {
            Err("Object is not of the asserted type")
        }
    }

    /// Get an `f64` slice associated with the object, or an error if the object is not of storage mode `double`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_double(self) -> Result<&'static mut [f64], &'static str> {
        Self::slice_mut(self, RObjectType::REALSXP, |x| unsafe { REAL(x) })
    }

    /// Get an `i32` slice associated with the object, or an error if the object is not of storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_integer(self) -> Result<&'static mut [i32], &'static str> {
        Self::slice_mut(self, RObjectType::INTSXP, |x| unsafe { INTEGER(x) })
    }

    /// Get an `i32` slice (of logical values) associated with the object, or an error if the object is not of storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_logical(self) -> Result<&'static mut [i32], &'static str> {
        Self::slice_mut(self, RObjectType::LGLSXP, |x| unsafe { LOGICAL(x) })
    }

    /// Get an `u8` slice associated with the object, or an error if the object is not of storage mode `raw`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_raw(self) -> Result<&'static mut [u8], &'static str> {
        Self::slice_mut(self, RObjectType::RAWSXP, |x| unsafe { RAW(x) })
    }

    fn coerce<T>(
        robject: Self,
        code: RObjectType,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> (Self, &'static mut [T]) {
        let sexp = if robject.tipe() != code {
            pc.protect(unsafe { Rf_coerceVector(robject.0 .0, code as u32) })
        } else {
            robject.0 .0
        };
        let len = robject.len();
        let slice = unsafe { std::slice::from_raw_parts_mut(get_ptr(sexp), len) };
        (robject, slice)
    }

    /// Coerce the object to storage mode `double` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_double(self, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        Self::coerce(self, RObjectType::REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Coerce the object to storage mode `integer` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_integer(self, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::coerce(self, RObjectType::INTSXP, |x| unsafe { INTEGER(x) }, pc)
    }

    /// Coerce the object to storage mode `logical` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_logical(self, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::coerce(self, RObjectType::LGLSXP, |x| unsafe { LOGICAL(x) }, pc)
    }

    /// Coerce the object to storage mode `raw` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_raw(self, pc: &mut Pc) -> (Self, &'static mut [u8]) {
        Self::coerce(self, RObjectType::RAWSXP, |x| unsafe { RAW(x) }, pc)
    }

    /// Coerce the object to storage mode `character`, or return an error if not possible.
    pub fn coerce_character(self, pc: &mut Pc) -> RVectorCharacter {
        let sexp = if self.tipe() != RObjectType::STRSXP {
            pc.protect(unsafe { Rf_coerceVector(self.0 .0, RObjectType::STRSXP as u32) })
        } else {
            self.0 .0
        };
        RVectorCharacter(RVector(RObject(sexp)))
    }

    /// Set the names attribute of an object.
    ///
    /// The function returns an error if `names` is not an vector object of the same length as the object.
    ///
    pub fn names_gets(self, names: RVectorCharacter) -> Result<(), &'static str> {
        if unsafe { Rf_length(names.0 .0 .0) != Rf_length(self.0 .0) } {
            Err("Lengths do not match")
        } else {
            unsafe { Rf_namesgets(self.0 .0, names.0 .0 .0) };
            Ok(())
        }
    }
}

impl Deref for RVector {
    type Target = RObject;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Character Vector

/// A Rust representation of an R character vector.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RVectorCharacter(RVector);

impl RVectorCharacter {
    /// Define a new vector with storage mode `character`.
    pub fn new(len: usize, pc: &mut Pc) -> Self {
        Self(RVector(RObject(unsafe {
            pc.protect(Rf_allocVector(
                RObjectType::STRSXP as u32,
                len.try_into().unwrap(),
            ))
        })))
    }

    /// Get an element of a character vector, with indexing starting at zero.
    ///
    /// This function returns an error if the object is not a character vector or if `i` is greater than or equal to the length of the vector.
    ///
    pub fn get(self, i: usize) -> Result<RObject, String> {
        let len = self.len();
        if i >= len {
            return Err(format!(
                "Index {} is out of bounds for vector of length {}",
                i, len
            ));
        }
        Ok(RObject(unsafe {
            STRING_ELT(self.0 .0 .0, i.try_into().unwrap())
        }))
    }

    /// Set an element of a character vector, with indexing starting at zero.
    ///
    /// This function returns an error if the object is not a character vector or if `i` is greater than or equal to the length of the vector.
    ///
    pub fn set(self, i: usize, value: &str, pc: &mut Pc) -> Result<(), String> {
        let len = self.len();
        if i >= len {
            return Err(format!(
                "Index {} is out of bounds for vector of length {}",
                i, len
            ));
        }
        unsafe {
            SET_STRING_ELT(
                self.0 .0 .0,
                i.try_into().unwrap(),
                RObject::new_character(value, pc).0,
            );
        };
        Ok(())
    }
}

impl Deref for RVectorCharacter {
    type Target = RVector;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// List

/// A Rust representation of an R list.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RList(RVector);

impl RList {
    /// Define a new list, i.e., a vector whose elements are of arbitrary type.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new(len: usize, pc: &mut Pc) -> Self {
        Self(RVector(RObject(pc.protect(unsafe {
            Rf_allocVector(RObjectType::VECSXP as u32, len.try_into().unwrap())
        }))))
    }

    /// Get an element of a list, with indexing starting at zero.
    ///
    /// This function returns an error if the object is not a list or if `i` is greater than or equal to the length of the list.
    ///
    pub fn get(self, i: usize) -> Result<RObject, String> {
        let len = self.len();
        if i >= len {
            return Err(format!(
                "Index {} is out of bounds for vector of length {}",
                i, len
            ));
        }
        Ok(RObject(unsafe {
            VECTOR_ELT(self.0 .0 .0, i.try_into().unwrap())
        }))
    }

    /// Set an element of a list, with indexing starting at zero.
    ///
    /// This function returns an error if the object is not a list or if `i` is greater than or equal to the length of the list.
    ///
    pub fn set(self, i: usize, value: impl Into<RObject>) -> Result<(), String> {
        let len = self.len();
        if i >= len {
            return Err(format!(
                "Index {} is out of bounds for vector of length {}",
                i, len
            ));
        }
        unsafe {
            SET_VECTOR_ELT(self.0 .0 .0, i.try_into().unwrap(), value.into().0);
        }
        Ok(())
    }
}

impl Deref for RList {
    type Target = RVector;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Matrix

/// A Rust representation of an R matrix.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RMatrix(RVector);

impl RMatrix {
    fn new_matrix<T>(
        nrow: usize,
        ncol: usize,
        code: RObjectType,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> (Self, &'static mut [T]) {
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                code as u32,
                nrow.try_into().unwrap(),
                ncol.try_into().unwrap(),
            ));
            let slice = std::slice::from_raw_parts_mut(get_ptr(sexp), nrow * ncol);
            (Self(RVector(RObject(sexp))), slice)
        }
    }

    /// Define a new matrix with storage mode `double`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_double(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        Self::new_matrix(nrow, ncol, RObjectType::REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Define a new matrix with storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_integer(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_matrix(
            nrow,
            ncol,
            RObjectType::INTSXP,
            |x| unsafe { INTEGER(x) },
            pc,
        )
    }

    /// Define a new matrix with storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_logical(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_matrix(
            nrow,
            ncol,
            RObjectType::LGLSXP,
            |x| unsafe { LOGICAL(x) },
            pc,
        )
    }

    /// Define a new matrix with storage mode `character`.
    pub fn new_character(nrow: usize, ncol: usize, pc: &mut Pc) -> Self {
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                RObjectType::STRSXP as u32,
                nrow.try_into().unwrap(),
                ncol.try_into().unwrap(),
            ));
            Self(RVector(RObject(sexp)))
        }
    }

    /// Coerce the object to storage mode `double` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_double(self, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        let (vec, slice) = (*self).coerce_double(pc);
        (Self(vec), slice)
    }

    /// Coerce the object to storage mode `integer` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_integer(self, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        let (vec, slice) = (*self).coerce_integer(pc);
        (Self(vec), slice)
    }

    /// Coerce the object to storage mode `logical` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_logical(self, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        let (vec, slice) = (*self).coerce_logical(pc);
        (Self(vec), slice)
    }

    /// Coerce the object to storage mode `raw` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_raw(self, pc: &mut Pc) -> (Self, &'static mut [u8]) {
        let (vec, slice) = (*self).coerce_raw(pc);
        (Self(vec), slice)
    }

    /// Get the number of rows in a matrix.
    ///
    /// The function returns an error if the object is not a matrix.
    ///
    pub fn nrow(self) -> usize {
        unsafe { Rf_nrows(self.0 .0 .0).try_into().unwrap() }
    }

    /// Get the number of columns in a matrix.
    ///
    /// The function returns an error if the object is not a matrix.
    ///
    pub fn ncol(self) -> usize {
        unsafe { Rf_ncols(self.0 .0 .0).try_into().unwrap() }
    }

    /// Is the object a square matrix?
    pub fn is_square(self) -> bool {
        self.nrow() == self.ncol()
    }

    /// Transpose a matrix.
    ///
    /// The function returns an error if the object is not a matrix.
    ///    
    pub fn transpose(self, pc: &mut Pc) -> Self {
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                self.tipe() as u32,
                Rf_ncols(self.0 .0 .0),
                Rf_nrows(self.0 .0 .0),
            ));
            Rf_copyMatrix(sexp, self.0 .0 .0, 1);
            Self(RVector(RObject(sexp)))
        }
    }
}

impl Deref for RMatrix {
    type Target = RVector;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Funct4ion

/// A Rust representation of an R function.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct RFunction(RObject);

impl RFunction {
    /// Call a function with no arguments, returning the value or an error code.
    pub fn call0(self, pc: &mut Pc) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang1(self.0 .0));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with one argument, returning the value or an error code.
    pub fn call1(self, x1: impl Into<RObject>, pc: &mut Pc) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang2(self.0 .0, x1.into().0));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with two arguments, returning the value or an error code.
    pub fn call2(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang3(self.0 .0, x1.into().0, x2.into().0));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with three arguments, returning the value or an error code.
    pub fn call3(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang4(self.0 .0, x1.into().0, x2.into().0, x3.into().0));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with four arguments, returning the value or an error code.
    pub fn call4(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        x4: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang5(
                self.0 .0,
                x1.into().0,
                x2.into().0,
                x3.into().0,
                x4.into().0,
            ));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with five arguments, returning the value or an error code.
    pub fn call5(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        x4: impl Into<RObject>,
        x5: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang6(
                self.0 .0,
                x1.into().0,
                x2.into().0,
                x3.into().0,
                x4.into().0,
                x5.into().0,
            ));
            Self(RObject(sexp)).eval(RObject(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with no arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call0`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call0_unsafe(self, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang1(self.0 .0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }

    /// Call a function with one argument.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call1`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call1_unsafe(self, x1: impl Into<RObject>, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang2(self.0 .0, x1.into().0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }

    /// Call a function with two arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call2`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call2_unsafe(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Self {
        let sexp = pc.protect(Rf_lang3(self.0 .0, x1.into().0, x2.into().0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }

    /// Call a function with three arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call3`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call3_unsafe(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Self {
        let sexp = pc.protect(Rf_lang4(self.0 .0, x1.into().0, x2.into().0, x3.into().0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }

    /// Call a function with four arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call4`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call4_unsafe(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        x4: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Self {
        let sexp = pc.protect(Rf_lang5(
            self.0 .0,
            x1.into().0,
            x2.into().0,
            x3.into().0,
            x4.into().0,
        ));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }

    /// Call a function with five arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call5`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call5_unsafe(
        self,
        x1: impl Into<RObject>,
        x2: impl Into<RObject>,
        x3: impl Into<RObject>,
        x4: impl Into<RObject>,
        x5: impl Into<RObject>,
        pc: &mut Pc,
    ) -> Self {
        let sexp = pc.protect(Rf_lang6(
            self.0 .0,
            x1.into().0,
            x2.into().0,
            x3.into().0,
            x4.into().0,
            x5.into().0,
        ));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(RObject(sexp))
    }
}

impl Deref for RFunction {
    type Target = RObject;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Conversion

pub trait AllocateProtected<T> {
    fn allocate(_: T, _: &mut Pc) -> Self;
}

pub trait TryAllocateProtected<T>: Sized {
    type Error;
    fn try_allocate(_: T, _: &mut Pc) -> Result<Self, Self::Error>;
}

// RObject

impl From<()> for RObject {
    fn from(_: ()) -> Self {
        RObject::nil()
    }
}

impl From<RVector> for RObject {
    fn from(x: RVector) -> Self {
        x.0
    }
}

impl From<RVectorCharacter> for RObject {
    fn from(x: RVectorCharacter) -> Self {
        x.0 .0
    }
}

impl From<RMatrix> for RObject {
    fn from(x: RMatrix) -> Self {
        x.0 .0
    }
}

impl From<RList> for RObject {
    fn from(x: RList) -> Self {
        x.0 .0
    }
}

impl From<RFunction> for RObject {
    fn from(x: RFunction) -> Self {
        x.0
    }
}

// f64

impl From<RObject> for f64 {
    fn from(x: RObject) -> f64 {
        unsafe { Rf_asReal(x.0) }
    }
}

impl AllocateProtected<f64> for RVector {
    fn allocate(x: f64, pc: &mut Pc) -> Self {
        Self(RObject(pc.protect(unsafe { Rf_ScalarReal(x) })))
    }
}

// i32

impl From<RObject> for i32 {
    fn from(x: RObject) -> i32 {
        unsafe { Rf_asInteger(x.0) }
    }
}

impl AllocateProtected<i32> for RVector {
    fn allocate(x: i32, pc: &mut Pc) -> Self {
        Self(RObject(pc.protect(unsafe { Rf_ScalarInteger(x) })))
    }
}

// bool

impl TryFrom<RObject> for bool {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<bool, Self::Error> {
        match unsafe { Rf_asLogical(x.0) } {
            x if x == Rboolean_TRUE.try_into().unwrap() => Ok(true),
            x if x == Rboolean_FALSE.try_into().unwrap() => Ok(false),
            _ => Err("Logical value is NA"),
        }
    }
}

impl AllocateProtected<bool> for RVector {
    fn allocate(x: bool, pc: &mut Pc) -> Self {
        Self(RObject(pc.protect(unsafe { Rf_ScalarLogical(x.into()) })))
    }
}

// usize

impl TryFrom<RObject> for usize {
    type Error = TryFromIntError;
    fn try_from(x: RObject) -> Result<usize, Self::Error> {
        usize::try_from(i32::from(x))
    }
}

impl TryAllocateProtected<usize> for RVector {
    type Error = TryFromIntError;
    fn try_allocate(x: usize, pc: &mut Pc) -> Result<Self, Self::Error> {
        match i32::try_from(x) {
            Ok(z) => Ok(Self::allocate(z, pc)),
            Err(e) => Err(e),
        }
    }
}

// Slices

impl TryFrom<RVector> for &[f64] {
    type Error = &'static str;
    fn try_from(x: RVector) -> Result<&'static [f64], &'static str> {
        x.slice_double()
    }
}

impl TryFrom<RVector> for &mut [f64] {
    type Error = &'static str;
    fn try_from(x: RVector) -> Result<&'static mut [f64], &'static str> {
        x.slice_mut_double()
    }
}

impl AllocateProtected<&[f64]> for RVector {
    fn allocate(x: &[f64], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_double(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl AllocateProtected<&mut [f64]> for RVector {
    fn allocate(x: &mut [f64], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_double(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl TryFrom<RVector> for &[i32] {
    type Error = &'static str;
    fn try_from(x: RVector) -> Result<&'static [i32], &'static str> {
        x.slice_integer()
    }
}

impl TryFrom<RVector> for &mut [i32] {
    type Error = &'static str;
    fn try_from(x: RVector) -> Result<&'static mut [i32], &'static str> {
        x.slice_mut_integer()
    }
}

impl AllocateProtected<&[i32]> for RVector {
    fn allocate(x: &[i32], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_integer(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl AllocateProtected<&mut [i32]> for RVector {
    fn allocate(x: &mut [i32], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_integer(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl TryAllocateProtected<&[usize]> for RVector {
    type Error = TryFromIntError;
    fn try_allocate(x: &[usize], pc: &mut Pc) -> Result<Self, Self::Error> {
        let (rvector, slice) = Self::new_integer(x.len(), pc);
        for (r, s) in slice.iter_mut().zip(x) {
            match i32::try_from(*s) {
                Ok(z) => *r = z,
                Err(e) => return Err(e),
            }
        }
        Ok(rvector)
    }
}

impl TryAllocateProtected<&mut [usize]> for RVector {
    type Error = TryFromIntError;
    fn try_allocate(x: &mut [usize], pc: &mut Pc) -> Result<Self, Self::Error> {
        let (robject, slice) = Self::new_integer(x.len(), pc);
        for (r, s) in slice.iter_mut().zip(x) {
            match i32::try_from(*s) {
                Ok(z) => *r = z,
                Err(e) => return Err(e),
            }
        }
        Ok(robject)
    }
}

impl TryFrom<RObject> for &[u8] {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<&'static [u8], &'static str> {
        let y = x.as_vector()?;
        y.slice_raw()
    }
}

impl TryFrom<RObject> for &mut [u8] {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<&'static mut [u8], &'static str> {
        let y = x.as_vector()?;
        y.slice_mut_raw()
    }
}

impl AllocateProtected<&[u8]> for RVector {
    fn allocate(x: &[u8], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_raw(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl AllocateProtected<&mut [u8]> for RVector {
    fn allocate(x: &mut [u8], pc: &mut Pc) -> Self {
        let (rvector, slice) = Self::new_raw(x.len(), pc);
        slice.copy_from_slice(x);
        rvector
    }
}

impl<const N: usize> AllocateProtected<[f64; N]> for RVector {
    fn allocate(x: [f64; N], pc: &mut Pc) -> Self {
        Self::allocate(&x[..], pc)
    }
}

impl<const N: usize> AllocateProtected<[i32; N]> for RVector {
    fn allocate(x: [i32; N], pc: &mut Pc) -> Self {
        Self::allocate(&x[..], pc)
    }
}

impl<const N: usize> AllocateProtected<[u8; N]> for RVector {
    fn allocate(x: [u8; N], pc: &mut Pc) -> Self {
        Self::allocate(&x[..], pc)
    }
}

// Pointers

impl TryFrom<RObject> for *const f64 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*const f64, &'static str> {
        if x.tipe() != RObjectType::REALSXP {
            Err("Object is not of storage mode `double`")
        } else {
            Ok(unsafe { REAL(x.0) })
        }
    }
}

impl TryFrom<RObject> for *mut f64 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*mut f64, &'static str> {
        if x.tipe() != RObjectType::REALSXP {
            Err("Object is not of storage mode `double`")
        } else {
            Ok(unsafe { REAL(x.0) })
        }
    }
}

impl TryFrom<RObject> for *const i32 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*const i32, &'static str> {
        let ft = x.tipe();
        if x.tipe() != RObjectType::INTSXP || ft != RObjectType::LGLSXP {
            Err("Object is not of storage mode `integer`")
        } else if ft == RObjectType::INTSXP {
            Ok(unsafe { INTEGER(x.0) })
        } else {
            Ok(unsafe { LOGICAL(x.0) })
        }
    }
}

impl TryFrom<RObject> for *mut i32 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*mut i32, &'static str> {
        let ft = x.tipe();
        if ft != RObjectType::INTSXP || ft != RObjectType::LGLSXP {
            Err("Object is not of storage mode `integer`")
        } else if ft == RObjectType::INTSXP {
            Ok(unsafe { INTEGER(x.0) })
        } else {
            Ok(unsafe { LOGICAL(x.0) })
        }
    }
}

impl TryFrom<RObject> for *const u8 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*const u8, &'static str> {
        if x.tipe() != RObjectType::RAWSXP {
            Err("Object is not of storage mode `raw`")
        } else {
            Ok(unsafe { RAW(x.0) })
        }
    }
}

impl TryFrom<RObject> for *mut u8 {
    type Error = &'static str;
    fn try_from(x: RObject) -> Result<*mut u8, &'static str> {
        if x.tipe() != RObjectType::RAWSXP {
            Err("Object is not of storage mode `raw`")
        } else {
            Ok(unsafe { RAW(x.0) })
        }
    }
}

// Characters

impl AllocateProtected<&str> for RVectorCharacter {
    fn allocate(x: &str, pc: &mut Pc) -> Self {
        let sexp = RObject::new_character(x, pc).0;
        Self(RVector(RObject(
            pc.protect(unsafe { Rf_ScalarString(sexp) }),
        )))
    }
}

impl AllocateProtected<&String> for RVectorCharacter {
    fn allocate(x: &String, pc: &mut Pc) -> Self {
        Self::allocate(&x[..], pc)
    }
}

impl AllocateProtected<String> for RVectorCharacter {
    fn allocate(x: String, pc: &mut Pc) -> Self {
        Self::allocate(&x[..], pc)
    }
}

impl<const LENGTH: usize> AllocateProtected<[&str; LENGTH]> for RVectorCharacter {
    fn allocate(x: [&str; LENGTH], pc: &mut Pc) -> Self {
        let rvector = Self::new(LENGTH, pc);
        for (i, x) in x.iter().enumerate() {
            unsafe {
                SET_STRING_ELT(
                    rvector.0 .0 .0,
                    i.try_into().unwrap(),
                    RObject::new_character(x, pc).0,
                )
            };
        }
        rvector
    }
}

impl AllocateProtected<&[&str]> for RVectorCharacter {
    fn allocate(x: &[&str], pc: &mut Pc) -> Self {
        let len = x.len();
        let rvector = Self::new(len, pc);
        for (i, x) in x.iter().enumerate() {
            unsafe {
                SET_STRING_ELT(
                    rvector.0 .0 .0,
                    i.try_into().unwrap(),
                    RObject::new_character(x, pc).0,
                )
            };
        }
        rvector
    }
}

impl TryFrom<RVectorCharacter> for &str {
    type Error = String;
    fn try_from(x: RVectorCharacter) -> Result<&'static str, String> {
        if x.is_empty() {
            return Err("Length must be at least one".to_string());
        }
        let sexp = x.get(0)?.0;
        let a = unsafe { R_CHAR(sexp) as *const c_char };
        let c_str = unsafe { CStr::from_ptr(a) };
        match c_str.to_str() {
            Ok(x) => Ok(x),
            Err(_) => Err("Non UTF-8".to_string()),
        }
    }
}

/// A protection counter to automatically unprotect R objects when this structure goes out of scope.
pub struct Pc {
    counter: i32,
}

impl Pc {
    /// Allocate a new protection counter.
    ///
    /// Functions defined with the `roxido` macro already have an instance of this structure named
    /// `pc`, so this function is generally not needed.
    ///
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    /// Protect an `SEXP` and increment the protection counter.
    ///
    /// This function protects an `SEXP`, a pointer to R's `SEXPREC` structure, and increments the
    /// protection counter.  This is generally only used when code directly calls functions in
    /// [`crate::rbindings`].
    ///
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn protect(&mut self, sexp: SEXP) -> SEXP {
        unsafe { Rf_protect(sexp) };
        self.counter += 1;
        sexp
    }
}

impl Default for Pc {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Pc {
    fn drop(&mut self) {
        if self.counter > 0 {
            unsafe { Rf_unprotect(self.counter) };
        }
    }
}

#[doc(hidden)]
#[no_mangle]
pub extern "C" fn set_custom_panic_hook() -> SEXP {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info.payload().downcast_ref::<RStopHelper>().is_none() {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
