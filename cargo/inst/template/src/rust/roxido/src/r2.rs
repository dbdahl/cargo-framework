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

use std::ffi::c_char;
use std::marker::PhantomData;

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
pub struct Character(());
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

    pub fn as_vector_i32(&self) -> Result<RObject<Vector, i32>, &'static str> {
        if self.is_vector_atomic() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 vector")
        }
    }

    pub fn as_matrix_f64(&self) -> Result<RObject<Matrix, f64>, &'static str> {
        if self.is_matrix() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 matrix")
        }
    }

    pub fn as_matrix_i32(&self) -> Result<RObject<Matrix, i32>, &'static str> {
        if self.is_matrix() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 matrix")
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

    /// Define a new element for a character vector.
    ///
    /// An element of a character vector should generally *not* be returned to a user, but this
    /// function can be used in conjunction with [`RVectorCharacter::set`].
    ///
    pub fn new_character(x: &str, pc: &mut Pc) -> RObject<Single, Character> {
        Self::new(pc.protect(unsafe {
            Rf_mkCharLen(x.as_ptr() as *const c_char, x.len().try_into().unwrap())
        }))
    }

    /// Define a new symbol.
    pub fn new_symbol(x: &str, pc: &mut Pc) -> RObject {
        let sexp = Self::new_character(x, pc).sexp;
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
    fn get(&self, index: usize) -> f64 {
        unsafe { REAL_ELT(self.sexp, index.try_into().unwrap()) }
    }

    fn set<RType, RMode>(&self, index: usize, value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, i32> {
    fn get(&self, index: usize) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, index.try_into().unwrap()) }
    }

    fn set<RType, RMode>(&self, index: usize, value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, index.try_into().unwrap(), value);
        }
    }
}

impl RObject<Vector, Unspecified> {
    fn get(&self, index: usize) -> RObject {
        Self::new(unsafe { VECTOR_ELT(self.sexp, index.try_into().unwrap()) })
    }

    fn set<RType, RMode>(&self, index: usize, value: RObject<RType, RMode>) {
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
    fn get(&self, index: (usize, usize)) -> f64 {
        unsafe { REAL_ELT(self.sexp, self.index(index)) }
    }

    fn set<RType, RMode>(&self, index: (usize, usize), value: f64) {
        unsafe {
            SET_REAL_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, i32> {
    fn get(&self, index: (usize, usize)) -> i32 {
        unsafe { INTEGER_ELT(self.sexp, self.index(index)) }
    }

    fn set<RType, RMode>(&self, index: (usize, usize), value: i32) {
        unsafe {
            SET_INTEGER_ELT(self.sexp, self.index(index), value);
        }
    }
}

impl RObject<Matrix, Unspecified> {
    fn get(&self, index: (usize, usize)) -> RObject {
        Self::new(unsafe { VECTOR_ELT(self.sexp, self.index(index)) })
    }

    fn set<RType, RMode>(&self, index: (usize, usize), value: RObject<RType, RMode>) {
        unsafe {
            SET_VECTOR_ELT(self.sexp, self.index(index), value.sexp);
        }
    }
}
