//! Extension Framework for R using Rust

#![allow(dead_code)]

// See:
//   https://cran.r-project.org/doc/manuals/r-release/R-ints.html
//   https://svn.r-project.org/R/trunk/src/include/Rinternals.h
//   https://github.com/hadley/r-internals

use crate::rbindings::*;
use std::convert::{TryFrom, TryInto};
use std::ffi::CStr;
use std::num::TryFromIntError;
use std::os::raw::{c_char, c_void};

/// A Rust representation of an R object.
///
/// Technically, this is simply a Rust new type idiom (newtype) for R's `SEXP`, a pointer to R's `SEXPREC` structure.
///
#[derive(Copy, Clone)]
#[repr(C)]
pub struct Rval(pub SEXP);

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

/// Print to the R console.
///
/// This is an implementation detail and *should not* be called directly!
/// This returns `true` if the print statement swallowed a user interrupt.
/// R checks for user interrupt every 100 print statements.
/// See the `Rvprintf` function in `printutils.c` of R's source.
///
#[doc(hidden)]
pub fn _print(x: &str, use_stdout: bool) -> bool {
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

impl Rval {
    fn new_vector<T>(
        len: usize,
        code: u32,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> (Rval, &'static mut [T]) {
        unsafe {
            let sexp = pc.protect(Rf_allocVector(code, len.try_into().unwrap()));
            let slice = std::slice::from_raw_parts_mut(get_ptr(sexp), len);
            (Rval(sexp), slice)
        }
    }

    fn new_matrix<T>(
        nrow: usize,
        ncol: usize,
        code: u32,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> (Rval, &'static mut [T]) {
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                code,
                nrow.try_into().unwrap(),
                ncol.try_into().unwrap(),
            ));
            let slice = std::slice::from_raw_parts_mut(get_ptr(sexp), nrow * ncol);
            (Rval(sexp), slice)
        }
    }

    /// Define a new vector with storage mode `double`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_vector_double(len: usize, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        Self::new_vector(len, REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Define a new vector with storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_vector_integer(len: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_vector(len, INTSXP, |x| unsafe { INTEGER(x) }, pc)
    }

    /// Define a new vector with storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_vector_logical(len: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_vector(len, LGLSXP, |x| unsafe { LOGICAL(x) }, pc)
    }

    /// Define a new vector with storage mode `character`.
    pub fn new_vector_character(len: usize, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_allocVector(STRSXP, len.try_into().unwrap()) }))
    }

    /// Define a new vector with storage mode `raw`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_vector_raw(len: usize, pc: &mut Pc) -> (Self, &'static mut [u8]) {
        Self::new_vector(len, RAWSXP, |x| unsafe { RAW(x) }, pc)
    }

    /// Define a new matrix with storage mode `double`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_matrix_double(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [f64]) {
        Self::new_matrix(nrow, ncol, REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Define a new matrix with storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_matrix_integer(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_matrix(nrow, ncol, INTSXP, |x| unsafe { INTEGER(x) }, pc)
    }

    /// Define a new matrix with storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn new_matrix_logical(nrow: usize, ncol: usize, pc: &mut Pc) -> (Self, &'static mut [i32]) {
        Self::new_matrix(nrow, ncol, LGLSXP, |x| unsafe { LOGICAL(x) }, pc)
    }

    /// Define a new matrix with storage mode `character`.
    pub fn new_matrix_character(nrow: usize, ncol: usize, pc: &mut Pc) -> Self {
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                STRSXP,
                nrow.try_into().unwrap(),
                ncol.try_into().unwrap(),
            ));
            Self(sexp)
        }
    }

    /// Define a new list.
    pub fn new_list(len: usize, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_allocVector(VECSXP, len.try_into().unwrap()) }))
    }

    /// Define a new error.
    ///
    /// This does *not* throw an error.  Instead, simply use `panic!`.
    ///
    pub fn new_error(message: &str, pc: &mut Pc) -> Self {
        let list = Self::new_list(2, pc);
        list.set_list_element(0, Self::new(message, pc));
        list.set_list_element(1, Self::nil());
        list.names_gets(Self::new(["message", "calls"], pc));
        list.class_gets(Self::new(["error", "condition"], pc));
        list
    }

    /// Define a new element for a character vector.
    ///
    /// An element of a character vector should generally *not* be returned to a user, but this
    /// function can be used in conjunction with [`Self::set_character_element`].
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

    /// Duplicate an object.
    ///
    /// Since multiple symbols may be bound to the same object, if the usual R semantics are to
    /// apply, any code which alters one of them needs to make a copy before modifying the copy.
    /// This method is commonly called on arguments to `.Call` before modifying them.
    ///
    pub fn duplicate(self, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_duplicate(self.0) }))
    }

    /// Move Rust object to an R external pointer
    ///
    /// This method moves a Rust object to an R external pointer and then, as far as Rust is concerned, leaks the memory.
    /// Thus the programmer is then responsible to release the memory by calling [`Self::external_pointer_decode`].
    ///
    pub fn external_pointer_encode<T>(x: T, tag: Self) -> Self {
        unsafe {
            // Move to Box<_> and then forget about it.
            let ptr = Box::into_raw(Box::new(x)) as *mut c_void;
            Self(R_MakeExternalPtr(ptr, tag.0, R_NilValue))
        }
    }

    /// Get tag for an R external pointer
    ///
    /// This method get the tag associated with an R external pointer, which was set by [`Self::external_pointer_encode`].
    ///
    pub fn external_pointer_tag(self) -> Self {
        unsafe { Rval(R_ExternalPtrTag(self.0)) }
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

    /// Get the value `NULL`.
    pub fn nil() -> Self {
        Self(unsafe { R_NilValue })
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

    /// Get R's definition of the `NA` value for an element of an object of storage mode `character`.
    pub fn na_character() -> Self {
        Self(unsafe { R_NaString })
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

    /// Test if value is NA for an element of an object of storage mode `character`.
    pub fn is_na_character(self) -> bool {
        unsafe { self.0 == R_NaString }
    }

    /// Is the object of storage mode `double`?
    pub fn is_double(self) -> bool {
        unsafe { Rf_isReal(self.0) != 0 }
    }

    /// Is the object of storage mode `integer`?
    pub fn is_integer(self) -> bool {
        unsafe { Rf_isInteger(self.0) != 0 }
    }

    /// Is the object of storage mode `double` or `integer`?
    pub fn is_double_or_integer(self) -> bool {
        self.is_double() || self.is_integer()
    }

    /// Is the object of storage mode `double` or `integer` and of length one?
    pub fn is_double_or_integer_scalar(self) -> bool {
        self.is_double_or_integer() && self.len() == 1
    }

    /// Is the object of storage mode `logical`?
    pub fn is_logical(self) -> bool {
        unsafe { Rf_isLogical(self.0) != 0 }
    }

    /// Is the object of storage mode `raw`?
    pub fn is_raw(self) -> bool {
        unsafe { TYPEOF(self.0) == RAWSXP.try_into().unwrap() }
    }

    /// Is the object a symbol?
    pub fn is_symbol(self) -> bool {
        unsafe { TYPEOF(self.0) == SYMSXP.try_into().unwrap() }
    }

    /// Is the object an element of an object of storage mode `character`?
    pub fn is_character_element(self) -> bool {
        unsafe { TYPEOF(self.0) == CHARSXP.try_into().unwrap() }
    }

    /// Is the object of storage mode `character`?
    pub fn is_character(self) -> bool {
        unsafe { Rf_isString(self.0) != 0 }
    }

    /// Is the object a list?
    pub fn is_list(self) -> bool {
        unsafe { TYPEOF(self.0) == VECSXP.try_into().unwrap() }
    }

    /// Is the object an atomic vector?
    pub fn is_vector_atomic(self) -> bool {
        unsafe { Rf_isVectorAtomic(self.0) != 0 }
    }

    /// Is the object a vector?
    pub fn is_vector(self) -> bool {
        unsafe { Rf_isVector(self.0) != 0 }
    }

    /// Is the object of length one?
    pub fn is_scalar(self) -> bool {
        self.len() == 1
    }

    /// Is the object a matrix?
    pub fn is_matrix(self) -> bool {
        unsafe { Rf_isMatrix(self.0) != 0 }
    }

    /// Is the object a square matrix?
    pub fn is_square_matrix(self) -> bool {
        self.is_matrix() && self.nrow() == self.ncol()
    }

    /// Is the object an array?
    pub fn is_array(self) -> bool {
        unsafe { Rf_isArray(self.0) != 0 }
    }

    /// Is the object a data.frame?
    pub fn is_data_frame(self) -> bool {
        unsafe { Rf_isFrame(self.0) != 0 }
    }

    /// Is the object `NULL`?
    pub fn is_nil(self) -> bool {
        unsafe { Rf_isNull(self.0) != 0 }
    }

    /// Is the object a function?
    pub fn is_function(self) -> bool {
        unsafe { Rf_isFunction(self.0) != 0 }
    }

    /// Is the object an environment?
    pub fn is_environment(self) -> bool {
        unsafe { Rf_isEnvironment(self.0) != 0 }
    }

    /// Can the object be interpreted as `TRUE`?
    pub fn is_true(self) -> bool {
        unsafe { Rf_asLogical(self.0) == Rboolean_TRUE.try_into().unwrap() }
    }

    /// Can the object be interpreted as `FALSE`?
    pub fn is_false(self) -> bool {
        unsafe { Rf_asLogical(self.0) == Rboolean_FALSE.try_into().unwrap() }
    }

    /// Get an element of a character vector, with indexing starting at zero.
    ///
    /// # Panics
    ///
    /// This function panics if the object is not a character vector or if `i` is greater than or equal to the length of the vector.
    ///
    pub fn get_character_element(self, i: usize) -> Self {
        if !self.is_character() {
            panic!("Not of storage mode `character`");
        }
        let len = self.len();
        if i >= len {
            panic!("Index {} is out of bounds for vector of length {}", i, len);
        }
        Self(unsafe { STRING_ELT(self.0, i.try_into().unwrap()) })
    }

    /// Set an element of a character vector, with indexing starting at zero.
    ///
    /// # Panics
    ///
    /// This function panics if the object is not a character vector or if `i` is greater than or equal to the length of the vector.
    ///
    pub fn set_character_element(self, i: usize, value: &str, pc: &mut Pc) {
        if !self.is_character() {
            panic!("Not of storage mode `character`");
        }
        let len = self.len();
        if i >= len {
            panic!("Index {} is out of bounds for vector of length {}", i, len);
        }
        unsafe {
            SET_STRING_ELT(
                self.0,
                i.try_into().unwrap(),
                Self::new_character(value, pc).0,
            );
        }
    }

    /// Get an element of a list, with indexing starting at zero.
    ///
    /// # Panics
    ///
    /// This function panics if the object is not a list or if `i` is greater than or equal to the length of the list.
    ///
    pub fn get_list_element(self, i: usize) -> Self {
        if !self.is_list() {
            panic!("Not a list");
        }
        let len = self.len();
        if i >= len {
            panic!("Index {} is out of bounds for list of length {}", i, len);
        }
        Self(unsafe { VECTOR_ELT(self.0, i.try_into().unwrap()) })
    }

    /// Set an element of a list, with indexing starting at zero.
    ///
    /// # Panics
    ///
    /// This function panics if the object is not a list or if `i` is greater than or equal to the length of the list.
    ///
    pub fn set_list_element(self, i: usize, value: Self) {
        if !self.is_list() {
            panic!("Not a list");
        }
        let len = self.len();
        if i >= len {
            panic!("Index {} is out of bounds for list of length {}", i, len);
        }
        unsafe {
            SET_VECTOR_ELT(self.0, i.try_into().unwrap(), value.0);
        }
    }

    /// Get an attribute.
    pub fn get_attribute(self, which: &str, pc: &mut Pc) -> Self {
        Self(unsafe { Rf_getAttrib(self.0, Self::new_symbol(which, pc).0) })
    }

    /// Set an attribute.
    pub fn set_attribute(self, which: &str, value: Self, pc: &mut Pc) {
        unsafe {
            Rf_setAttrib(self.0, Self::new_symbol(which, pc).0, value.0);
        }
    }

    /// Create a new binding (or changes the value of an existing binding) in the specified environment frame.  It is the analogue of `assign(self, value, envir = environment, inherits = FALSE)`.
    ///
    /// # Panics
    ///
    /// The function panics if the object is not a symbol.
    ///   
    pub fn assign(self, value: Self, environment: Self) {
        if !self.is_symbol() {
            panic!("Not a symbol")
        }
        unsafe { Rf_defineVar(self.0, value.0, environment.0) };
    }

    /// Search for an existing binding in the specified environment or its enclosing environments.  If a binding is found, its value is changed to `value`.  Otherwise, a new binding is created in the global environment.  This corresponds to `assign(self, value, envir = environment, inherits = TRUE)`.
    ///
    /// # Panics
    ///
    /// The function panics if the object is not a symbol.
    ///   
    pub fn assign_inherits(self, value: Self, environment: Self) {
        if !self.is_symbol() {
            panic!("Not a symbol")
        }
        unsafe { Rf_setVar(self.0, value.0, environment.0) };
    }

    /// Set the names attribute of an object.
    ///
    /// # Panics
    ///
    /// The function panics if `names` is not an vector object of the same length as the object.
    ///
    pub fn names_gets(self, names: Self) {
        if !self.is_vector() {
            panic!("Not a vector")
        }
        if names.len() != self.len() {
            panic!("'names' is not the same length as vector")
        }
        unsafe { Rf_namesgets(self.0, names.0) };
    }

    /// Set the class attribute of an object.
    ///
    /// # Panics
    ///
    /// The function panics if `value` is not a character vector.
    ///
    pub fn class_gets(self, value: Self) {
        if !value.is_character() {
            panic!("'value' is not a character vector")
        }
        unsafe { Rf_classgets(self.0, value.0) };
    }

    /// Get the length of the object.
    pub fn len(self) -> usize {
        unsafe { Rf_length(self.0).try_into().unwrap() }
    }

    /// Is the length of the object zero?
    pub fn is_empty(self) -> bool {
        unsafe { Rf_length(self.0) == 0 }
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

    /// Coerce the object to storage mode `character` and get the associated `String` value.
    ///
    /// If coercion is not possible (because, for example, no UTF-8 representation exists), `""` is returned.
    ///
    pub fn as_string(self) -> String {
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(self.0)) as *const c_char) };
        match c_str.to_str() {
            Ok(x) => x.to_string(),
            Err(_) => "".to_string(),
        }
    }

    /// Coerce the object to storage mode `character` and get the associated `&str` value.
    ///
    /// If coercion is not possible (because, for example, no UTF-8 representation exists), `""` is returned.
    ///
    pub fn as_str(self) -> &'static str {
        let c_str = unsafe { CStr::from_ptr(R_CHAR(Rf_asChar(self.0)) as *const c_char) };
        match c_str.to_str() {
            Ok(x) => x,
            Err(_) => "",
        }
    }

    /// Get the number of rows in a matrix.
    ///
    /// # Panics
    ///
    /// The function panics if the object is not a matrix.
    ///
    pub fn nrow(self) -> usize {
        if !self.is_matrix() {
            panic!("Not a matrix");
        }
        unsafe { Rf_nrows(self.0).try_into().unwrap() }
    }

    /// Get the number of columns in a matrix.
    ///
    /// # Panics
    ///
    /// The function panics if the object is not a matrix.
    ///
    pub fn ncol(self) -> usize {
        if !self.is_matrix() {
            panic!("Not a matrix");
        }
        unsafe { Rf_ncols(self.0).try_into().unwrap() }
    }

    /// Transpose a matrix.
    ///
    /// # Panics
    ///
    /// The function panics if the object is not a matrix.
    ///    
    pub fn transpose(self, pc: &mut Pc) -> Self {
        if !self.is_matrix() {
            panic!("Not a matrix");
        }
        unsafe {
            let sexp = pc.protect(Rf_allocMatrix(
                TYPEOF(self.0).try_into().unwrap(),
                Rf_ncols(self.0),
                Rf_nrows(self.0),
            ));
            Rf_copyMatrix(sexp, self.0, 1);
            Self(sexp)
        }
    }

    fn slice<T>(
        rval: Self,
        code: u32,
        get_ptr: impl Fn(SEXP) -> *mut T,
    ) -> Result<&'static [T], &'static str> {
        let ft = unsafe { TYPEOF(rval.0) } as u32;
        if ft == code {
            let len = rval.len();
            let slice = unsafe { std::slice::from_raw_parts(get_ptr(rval.0), len) };
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
        Self::slice(self, REALSXP, |x| unsafe { REAL(x) })
    }

    /// Get an `i32` slice associated with the object, or an error if the object is not of storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_integer(self) -> Result<&'static [i32], &'static str> {
        Self::slice(self, INTSXP, |x| unsafe { INTEGER(x) })
    }

    /// Get an `i32` slice (of logical values) associated with the object, or an error if the object is not of storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_logical(self) -> Result<&'static [i32], &'static str> {
        Self::slice(self, LGLSXP, |x| unsafe { LOGICAL(x) })
    }

    /// Get an `u8` slice associated with the object, or an error if the object is not of storage mode `raw`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_raw(self) -> Result<&'static [u8], &'static str> {
        Self::slice(self, RAWSXP, |x| unsafe { RAW(x) })
    }


    fn slice_mut<T>(
        rval: Self,
        code: u32,
        get_ptr: impl Fn(SEXP) -> *mut T,
    ) -> Result<&'static mut [T], &'static str> {
        let ft = unsafe { TYPEOF(rval.0) } as u32;
        if ft == code {
            let len = rval.len();
            let slice = unsafe { std::slice::from_raw_parts_mut(get_ptr(rval.0), len) };
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
        Self::slice_mut(self, REALSXP, |x| unsafe { REAL(x) })
    }

    /// Get an `i32` slice associated with the object, or an error if the object is not of storage mode `integer`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_integer(self) -> Result<&'static mut [i32], &'static str> {
        Self::slice_mut(self, INTSXP, |x| unsafe { INTEGER(x) })
    }

    /// Get an `i32` slice (of logical values) associated with the object, or an error if the object is not of storage mode `logical`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_logical(self) -> Result<&'static mut [i32], &'static str> {
        Self::slice_mut(self, LGLSXP, |x| unsafe { LOGICAL(x) })
    }

    /// Get an `u8` slice associated with the object, or an error if the object is not of storage mode `raw`.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn slice_mut_raw(self) -> Result<&'static mut [u8], &'static str> {
        Self::slice_mut(self, RAWSXP, |x| unsafe { RAW(x) })
    }

    fn coerce<T>(
        rval: Self,
        code: u32,
        get_ptr: impl Fn(SEXP) -> *mut T,
        pc: &mut Pc,
    ) -> Result<(Self, &'static mut [T]), &'static str> {
        let ft = unsafe { TYPEOF(rval.0) } as u32;
        if ft == REALSXP || ft == INTSXP || ft == LGLSXP || ft == STRSXP || ft == NILSXP {
            let rval = if ft != code {
                let sexp = pc.protect(unsafe { Rf_coerceVector(rval.0, code) });
                Self(sexp)
            } else {
                rval
            };
            let len = rval.len();
            let slice = unsafe { std::slice::from_raw_parts_mut(get_ptr(rval.0), len) };
            Ok((rval, slice))
        } else {
            Err("Object is not of the asserted type")
        }
    }

    /// Coerce the object to storage mode `double` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_double(self, pc: &mut Pc) -> Result<(Self, &'static mut [f64]), &'static str> {
        Self::coerce(self, REALSXP, |x| unsafe { REAL(x) }, pc)
    }

    /// Coerce the object to storage mode `integer` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_integer(self, pc: &mut Pc) -> Result<(Self, &'static mut [i32]), &'static str> {
        Self::coerce(self, INTSXP, |x| unsafe { INTEGER(x) }, pc)
    }

    /// Coerce the object to storage mode `logical` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_logical(self, pc: &mut Pc) -> Result<(Self, &'static mut [i32]), &'static str> {
        Self::coerce(self, LGLSXP, |x| unsafe { LOGICAL(x) }, pc)
    }

    /// Coerce the object to storage mode `raw` and get the associated slice, or return an error if not possible.
    ///
    /// Although the stated lifetime is `'static`, the reference is actually only valid as long as
    /// the associated R object exists.
    ///
    pub fn coerce_raw(self, pc: &mut Pc) -> Result<(Self, &'static mut [u8]), &'static str> {
        Self::coerce(self, RAWSXP, |x| unsafe { RAW(x) }, pc)
    }

    /// Coerce the object to storage mode `character`, or return an error if not possible.
    pub fn coerce_character(self, pc: &mut Pc) -> Result<Self, &'static str> {
        let ft = unsafe { TYPEOF(self.0) } as u32;
        if ft == REALSXP || ft == INTSXP || ft == LGLSXP || ft == STRSXP || ft == NILSXP {
            let rval = if ft != STRSXP {
                let sexp = pc.protect(unsafe { Rf_coerceVector(self.0, STRSXP) });
                Self(sexp)
            } else {
                self
            };
            Ok(rval)
        } else {
            Err("object is not of the asserted type")
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
        let sexp = pc.protect(Rf_lang1(self.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
    }

    /// Call a function with one argument.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call1`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call1_unsafe(self, x1: Self, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang2(self.0, x1.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
    }

    /// Call a function with two arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call2`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call2_unsafe(self, x1: Self, x2: Self, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang3(self.0, x1.0, x2.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
    }

    /// Call a function with three arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call3`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call3_unsafe(self, x1: Self, x2: Self, x3: Self, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang4(self.0, x1.0, x2.0, x3.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
    }

    /// Call a function with four arguments.
    ///
    /// # Safety
    ///
    /// This function will cause a long jump (leading to a memory leak) if the R function throws any errors.
    /// Using [`Self::call4`] is the safe alternative, with slightly more overhead.
    ///
    pub unsafe fn call4_unsafe(self, x1: Self, x2: Self, x3: Self, x4: Self, pc: &mut Pc) -> Self {
        let sexp = pc.protect(Rf_lang5(self.0, x1.0, x2.0, x3.0, x4.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
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
        x1: Self,
        x2: Self,
        x3: Self,
        x4: Self,
        x5: Self,
        pc: &mut Pc,
    ) -> Self {
        let sexp = pc.protect(Rf_lang6(self.0, x1.0, x2.0, x3.0, x4.0, x5.0));
        let sexp = pc.protect(Rf_eval(sexp, R_GetCurrentEnv()));
        Self(sexp)
    }

    /// Evaluation a expression, returning the value or an error code.
    pub fn eval(self, environment: Self, pc: &mut Pc) -> Result<Self, i32> {
        let mut p_out_error: i32 = 0;
        let sexp = unsafe { R_tryEval(self.0, environment.0, &mut p_out_error as *mut i32) };
        match p_out_error {
            0 => Ok(Self(pc.protect(sexp))),
            e => Err(e),
        }
    }

    /// Call a function with no arguments, returning the value or an error code.
    pub fn call0(self, pc: &mut Pc) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang1(self.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with one argument, returning the value or an error code.
    pub fn call1(self, x1: Self, pc: &mut Pc) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang2(self.0, x1.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with two arguments, returning the value or an error code.
    pub fn call2(self, x1: Self, x2: Self, pc: &mut Pc) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang3(self.0, x1.0, x2.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with three arguments, returning the value or an error code.
    pub fn call3(self, x1: Self, x2: Self, x3: Self, pc: &mut Pc) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang4(self.0, x1.0, x2.0, x3.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with four arguments, returning the value or an error code.
    pub fn call4(self, x1: Self, x2: Self, x3: Self, x4: Self, pc: &mut Pc) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang5(self.0, x1.0, x2.0, x3.0, x4.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }

    /// Call a function with five arguments, returning the value or an error code.
    pub fn call5(
        self,
        x1: Self,
        x2: Self,
        x3: Self,
        x4: Self,
        x5: Self,
        pc: &mut Pc,
    ) -> Result<Self, i32> {
        unsafe {
            let sexp = pc.protect(Rf_lang6(self.0, x1.0, x2.0, x3.0, x4.0, x5.0));
            Self(sexp).eval(Self(R_GetCurrentEnv()), pc)
        }
    }
}

// Conversion

pub trait NewProtected<T> {
    fn new(_: T, _: &mut Pc) -> Self;
}

pub trait TryNewProtected<T>: Sized {
    type Error;
    fn try_new(_: T, _: &mut Pc) -> Result<Self, Self::Error>;
}

// f64

impl From<Rval> for f64 {
    fn from(x: Rval) -> f64 {
        unsafe { Rf_asReal(x.0) }
    }
}

impl NewProtected<f64> for Rval {
    fn new(x: f64, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_ScalarReal(x) }))
    }
}

// i32

impl From<Rval> for i32 {
    fn from(x: Rval) -> i32 {
        unsafe { Rf_asInteger(x.0) }
    }
}

impl NewProtected<i32> for Rval {
    fn new(x: i32, pc: &mut Pc) -> Self {
        Self(pc.protect(unsafe { Rf_ScalarInteger(x) }))
    }
}

// bool

impl TryFrom<Rval> for bool {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<bool, Self::Error> {
        match unsafe { Rf_asLogical(x.0) } {
            x if x == Rboolean_TRUE.try_into().unwrap() => Ok(true),
            x if x == Rboolean_FALSE.try_into().unwrap() => Ok(false),
            _ => Err("Logical value is NA"),
        }
    }
}

impl NewProtected<bool> for Rval {
    fn new(x: bool, pc: &mut Pc) -> Self {
        Rval(pc.protect(unsafe { Rf_ScalarLogical(x.into()) }))
    }
}

// usize

impl TryFrom<Rval> for usize {
    type Error = TryFromIntError;
    fn try_from(x: Rval) -> Result<usize, Self::Error> {
        usize::try_from(i32::from(x))
    }
}

impl TryNewProtected<usize> for Rval {
    type Error = TryFromIntError;
    fn try_new(x: usize, pc: &mut Pc) -> Result<Self, Self::Error> {
        match i32::try_from(x) {
            Ok(z) => Ok(Rval::new(z, pc)),
            Err(e) => Err(e),
        }
    }
}

// Slices

impl TryFrom<Rval> for &[f64] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static [f64], &'static str> {
        x.slice_double()
    }
}

impl TryFrom<Rval> for &mut [f64] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static mut [f64], &'static str> {
        x.slice_mut_double()
    }
}

impl NewProtected<&[f64]> for Rval {
    fn new(x: &[f64], pc: &mut Pc) -> Self {
        let (rval, slice) = Rval::new_vector_double(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl NewProtected<&mut [f64]> for Rval {
    fn new(x: &mut [f64], pc: &mut Pc) -> Self {
        let (rval, slice) = Rval::new_vector_double(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl TryFrom<Rval> for &[i32] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static [i32], &'static str> {
        x.slice_integer()
    }
}

impl TryFrom<Rval> for &mut [i32] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static mut [i32], &'static str> {
        x.slice_mut_integer()
    }
}

impl NewProtected<&[i32]> for Rval {
    fn new(x: &[i32], pc: &mut Pc) -> Self {
        let (rval, slice) = Rval::new_vector_integer(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl NewProtected<&mut [i32]> for Rval {
    fn new(x: &mut [i32], pc: &mut Pc) -> Self {
        let (rval, slice) = Rval::new_vector_integer(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl TryNewProtected<&[usize]> for Rval {
    type Error = TryFromIntError;
    fn try_new(x: &[usize], pc: &mut Pc) -> Result<Self, Self::Error> {
        let (rval, slice) = Rval::new_vector_integer(x.len(), pc);
        for (r, s) in slice.iter_mut().zip(x) {
            match i32::try_from(*s) {
                Ok(z) => *r = z,
                Err(e) => return Err(e),
            }
        }
        Ok(rval)
    }
}

impl TryNewProtected<&mut [usize]> for Rval {
    type Error = TryFromIntError;
    fn try_new(x: &mut [usize], pc: &mut Pc) -> Result<Self, Self::Error> {
        let (rval, slice) = Rval::new_vector_integer(x.len(), pc);
        for (r, s) in slice.iter_mut().zip(x) {
            match i32::try_from(*s) {
                Ok(z) => *r = z,
                Err(e) => return Err(e),
            }
        }
        Ok(rval)
    }
}

impl TryFrom<Rval> for &[u8] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static [u8], &'static str> {
        x.slice_raw()
    }
}

impl TryFrom<Rval> for &mut [u8] {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static mut [u8], &'static str> {
        x.slice_mut_raw()
    }
}

impl NewProtected<&[u8]> for Rval {
    fn new(x: &[u8], pc: &mut Pc) -> Rval {
        let (rval, slice) = Rval::new_vector_raw(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl NewProtected<&mut [u8]> for Rval {
    fn new(x: &mut [u8], pc: &mut Pc) -> Rval {
        let (rval, slice) = Rval::new_vector_raw(x.len(), pc);
        slice.copy_from_slice(x);
        rval
    }
}

impl<const N: usize> NewProtected<[f64; N]> for Rval {
    fn new(x: [f64; N], pc: &mut Pc) -> Self {
        Self::new(&x[..], pc)
    }
}

impl<const N: usize> NewProtected<[i32; N]> for Rval {
    fn new(x: [i32; N], pc: &mut Pc) -> Self {
        Self::new(&x[..], pc)
    }
}

impl<const N: usize> NewProtected<[u8; N]> for Rval {
    fn new(x: [u8; N], pc: &mut Pc) -> Self {
        Self::new(&x[..], pc)
    }
}

// Pointers

impl TryFrom<Rval> for *const f64 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*const f64, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != REALSXP {
            Err("Object is not of storage mode `double`")
        } else {
            Ok(unsafe { REAL(x.0) })
        }
    }
}

impl TryFrom<Rval> for *mut f64 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*mut f64, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != REALSXP {
            Err("Object is not of storage mode `double`")
        } else {
            Ok(unsafe { REAL(x.0) })
        }
    }
}

impl TryFrom<Rval> for *const i32 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*const i32, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != INTSXP || ft != LGLSXP {
            Err("Object is not of storage mode `integer`")
        } else if ft == INTSXP {
            Ok(unsafe { INTEGER(x.0) })
        } else {
            Ok(unsafe { LOGICAL(x.0) })
        }
    }
}

impl TryFrom<Rval> for *mut i32 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*mut i32, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != INTSXP || ft != LGLSXP {
            Err("Object is not of storage mode `integer`")
        } else if ft == INTSXP {
            Ok(unsafe { INTEGER(x.0) })
        } else {
            Ok(unsafe { LOGICAL(x.0) })
        }
    }
}

impl TryFrom<Rval> for *const u8 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*const u8, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != RAWSXP {
            Err("Object is not of storage mode `raw`")
        } else {
            Ok(unsafe { RAW(x.0) })
        }
    }
}

impl TryFrom<Rval> for *mut u8 {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<*mut u8, &'static str> {
        let ft = unsafe { TYPEOF(x.0) } as u32;
        if ft != RAWSXP {
            Err("Object is not of storage mode `raw`")
        } else {
            Ok(unsafe { RAW(x.0) })
        }
    }
}

// Characters

impl NewProtected<&str> for Rval {
    fn new(x: &str, pc: &mut Pc) -> Self {
        let sexp = Self::new_character(x, pc).0;
        Self(pc.protect(unsafe { Rf_ScalarString(sexp) }))
    }
}

impl NewProtected<&String> for Rval {
    fn new(x: &String, pc: &mut Pc) -> Self {
        Self::new(&x[..], pc)
    }
}

impl NewProtected<String> for Rval {
    fn new(x: String, pc: &mut Pc) -> Self {
        Self::new(&x[..], pc)
    }
}

impl<const LENGTH: usize> NewProtected<[&str; LENGTH]> for Rval {
    fn new(x: [&str; LENGTH], pc: &mut Pc) -> Self {
        let y = Rval::new_vector_character(LENGTH, pc);
        for (i, x) in x.iter().enumerate() {
            unsafe { SET_STRING_ELT(y.0, i.try_into().unwrap(), Self::new_character(*x, pc).0) };
        }
        y
    }
}

impl NewProtected<&[&str]> for Rval {
    fn new(x: &[&str], pc: &mut Pc) -> Self {
        let len = x.len();
        let y = Rval::new_vector_character(len, pc);
        for (i, x) in x.iter().enumerate() {
            unsafe { SET_STRING_ELT(y.0, i.try_into().unwrap(), Self::new_character(*x, pc).0) };
        }
        y
    }
}

impl TryFrom<Rval> for &str {
    type Error = &'static str;
    fn try_from(x: Rval) -> Result<&'static str, &'static str> {
        let sexp = if x.is_character() {
            if x.is_empty() {
                return Err("Length must be at least one");
            }
            x.get_character_element(0).0
        } else {
            x.0
        };
        if unsafe { TYPEOF(sexp) != CHARSXP.try_into().unwrap() } {
            return Err("Object is not of storage model `character`");
        }
        let a = unsafe { R_CHAR(sexp) as *const c_char };
        let c_str = unsafe { CStr::from_ptr(a) };
        match c_str.to_str() {
            Ok(x) => Ok(x),
            Err(_) => Err("Could not convert to UTF-8"),
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
    std::panic::set_hook(Box::new(|panic_info| {
        let (filename, line, column) = if let Some(location) = panic_info.location() {
            (
                location.file().to_string(),
                location.line(),
                location.column(),
            )
        } else {
            ("".to_string(), 0, 0)
        };
        let message = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = panic_info.payload().downcast_ref::<String>() {
            s.to_string()
        } else {
            "".to_string()
        };
        use crate::r;
        reprintln!(
            "Error in file '{}' at line {}, column {} : {}",
            filename,
            line,
            column,
            message
        );
    }));
    unsafe { R_NilValue }
}
