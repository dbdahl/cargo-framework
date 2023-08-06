use crate::pc::Pc;
use crate::rbindings::*;

use std::ffi::c_char;
use std::marker::PhantomData;
use std::ops::Index;

struct AnyType(());
struct Vector(());
struct Matrix(());
struct Array(());
struct Function(());
struct RString(());

struct TF64(());
struct TI32(());
struct Character(());
struct Unspecified(());

trait Sliceable {}

impl Sliceable for Vector {}
impl Sliceable for Matrix {}
impl Sliceable for Array {}

trait Numeric {}

impl Numeric for TF64 {}
impl Numeric for TI32 {}

struct RObject<RType = AnyType, RMode = Unspecified> {
    ptr: SEXP,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {
    fn new<RTypeTo, RModeTo>(ptr: SEXP) -> RObject<RTypeTo, RModeTo> {
        RObject {
            ptr,
            rtype: PhantomData,
        }
    }

    fn convert<RTypeTo, RModeTo>(self) -> RObject<RTypeTo, RModeTo> {
        Self::new(self.ptr)
        // RObject {
        //     ptr: self.ptr,
        //     rtype: PhantomData,
        // }
    }

    pub fn to_base(self) -> RObject {
        self.convert()
    }

    pub fn is_f64(&self) -> bool {
        unsafe { Rf_isReal(self.ptr) != 0 }
    }

    pub fn is_i32(&self) -> bool {
        unsafe { Rf_isInteger(self.ptr) != 0 }
    }

    pub fn is_vector(&self) -> bool {
        unsafe { Rf_isVector(self.ptr) != 0 }
    }

    pub fn is_vector_atomic(&self) -> bool {
        unsafe { Rf_isVectorAtomic(self.ptr) != 0 }
    }

    pub fn is_vector_list(&self) -> bool {
        unsafe { Rf_isVectorList(self.ptr) != 0 }
    }

    pub fn is_matrix(&self) -> bool {
        unsafe { Rf_isMatrix(self.ptr) != 0 }
    }

    pub fn is_array(&self) -> bool {
        unsafe { Rf_isArray(self.ptr) != 0 }
    }

    pub fn is_function(&self) -> bool {
        unsafe { Rf_isFunction(self.ptr) != 0 }
    }

    pub fn is_number(&self) -> bool {
        unsafe { Rf_isNumber(self.ptr) != 0 }
    }

    pub fn is_scalar(&self) -> bool {
        if self.is_number() {
            unsafe { Rf_xlength(self.ptr) == 1 }
        } else {
            false
        }
    }

    pub fn as_f64(&self) -> Result<f64, &'static str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asReal(self.ptr) })
        } else {
            Err("Canot be interperated as an f64")
        }
    }

    pub fn as_i32(&self) -> Result<i32, &'static str> {
        if self.is_scalar() {
            Ok(unsafe { Rf_asInteger(self.ptr) })
        } else {
            Err("Canot be interperated as an i32")
        }
    }

    pub fn as_usize(&self) -> Result<usize, &'static str> {
        if self.is_scalar() {
            let value = unsafe { Rf_asInteger(self.ptr) };
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
            let value = unsafe { Rf_asLogical(self.ptr) };
            Ok(value != 0)
        } else {
            Err("Canot be interperated as an bool")
        }
    }

    pub fn as_vector_f64(self) -> Result<RObject<Vector, TF64>, &'static str> {
        if self.is_vector_atomic() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 vector")
        }
    }

    pub fn as_vector_integer(self) -> Result<RObject<Vector, TI32>, &'static str> {
        if self.is_vector_atomic() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 vector")
        }
    }

    pub fn as_matrix_f64(self) -> Result<RObject<Matrix, TF64>, &'static str> {
        if self.is_matrix() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 matrix")
        }
    }

    pub fn as_matrix_i32(self) -> Result<RObject<Matrix, TI32>, &'static str> {
        if self.is_matrix() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 matrix")
        }
    }

    pub fn as_array_f64(self) -> Result<RObject<Array, TF64>, &'static str> {
        if self.is_array() && self.is_f64() {
            Ok(self.convert())
        } else {
            Err("Not an f64 array")
        }
    }

    pub fn as_array_i32(self) -> Result<RObject<Array, TI32>, &'static str> {
        if self.is_array() && self.is_i32() {
            Ok(self.convert())
        } else {
            Err("Not an i32 array")
        }
    }

    pub fn as_vector_list(self) -> Result<RObject<Vector, Unspecified>, &'static str> {
        if self.is_vector_list() {
            Ok(self.convert())
        } else {
            Err("Not a vector list")
        }
    }

    pub fn as_function(self) -> Result<RObject<Function, Unspecified>, &'static str> {
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
    pub fn new_character(x: &str, pc: &mut Pc) -> RObject<RString, Character> {
        Self::new(pc.protect(unsafe {
            Rf_mkCharLen(x.as_ptr() as *const c_char, x.len().try_into().unwrap())
        }))
    }

    /// Define a new symbol.
    pub fn new_symbol(x: &str, pc: &mut Pc) -> RObject {
        let sexp = Self::new_character(x, pc).ptr;
        Self::new(pc.protect(unsafe { Rf_installChar(sexp) }))
    }

    /// Get an attribute.
    pub fn get_attribute(self, which: &str, pc: &mut Pc) -> RObject {
        Self::new(unsafe { Rf_getAttrib(self.ptr, Self::new_symbol(which, pc).ptr) })
    }

    /// Set an attribute.
    pub fn set_attribute(self, which: &str, value: impl Into<RObject<RType, RMode>>, pc: &mut Pc) {
        unsafe {
            Rf_setAttrib(self.ptr, Self::new_symbol(which, pc).ptr, value.into().ptr);
        }
    }
}

impl<S: Sliceable, T> RObject<S, T> {
    pub fn len(&self) -> usize {
        let len = unsafe { Rf_xlength(self.ptr) };
        len.try_into().unwrap() // Won't ever fail if R is sane.
    }

    fn slice_engine<U>(&self, data: *mut U) -> &'static [U] {
        let len = self.len();
        unsafe { std::slice::from_raw_parts_mut(data, len) }
    }
}

impl<S: Sliceable> RObject<S, TF64> {
    pub fn slice(&self) -> &'static [f64] {
        self.slice_engine(unsafe { REAL(self.ptr) })
    }
}

impl<S: Sliceable> RObject<S, TI32> {
    pub fn slice(&self) -> &'static [i32] {
        self.slice_engine(unsafe { INTEGER(self.ptr) })
    }
}

impl<T> RObject<Matrix, T> {
    pub fn nrows(&self) -> usize {
        unsafe { Rf_nrows(self.ptr).try_into().unwrap() }
    }
    pub fn ncols(&self) -> usize {
        unsafe { Rf_ncols(self.ptr).try_into().unwrap() }
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
                    ptr: sexp,
                    rtype: PhantomData,
                };
                Ok(robject)
            }
            e => Err(e),
        }
    }

    pub fn call0(self, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang1(self.ptr) };
        Self::eval(expression, pc)
    }

    pub fn call1<T1, M1>(self, arg1: RObject<T1, M1>, pc: &mut Pc) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang2(self.ptr, arg1.ptr) };
        Self::eval(expression, pc)
    }

    pub fn call2<T1, M1, T2, M2>(
        self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang3(self.ptr, arg1.ptr, arg2.ptr) };
        Self::eval(expression, pc)
    }

    pub fn call3<T1, M1, T2, M2, T3, M3>(
        self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang4(self.ptr, arg1.ptr, arg2.ptr, arg3.ptr) };
        Self::eval(expression, pc)
    }

    pub fn call4<T1, M1, T2, M2, T3, M3, T4, M4>(
        self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        arg4: RObject<T4, M4>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression = unsafe { Rf_lang5(self.ptr, arg1.ptr, arg2.ptr, arg3.ptr, arg4.ptr) };
        Self::eval(expression, pc)
    }

    pub fn call5<T1, M1, T2, M2, T3, M3, T4, M4, T5, M5>(
        self,
        arg1: RObject<T1, M1>,
        arg2: RObject<T2, M2>,
        arg3: RObject<T2, M3>,
        arg4: RObject<T4, M4>,
        arg5: RObject<T5, M5>,
        pc: &mut Pc,
    ) -> Result<RObject, i32> {
        let expression =
            unsafe { Rf_lang6(self.ptr, arg1.ptr, arg2.ptr, arg3.ptr, arg4.ptr, arg5.ptr) };
        Self::eval(expression, pc)
    }
}

impl Index<RObject<Vector, TF64>> for usize {
    type Output = f64;

    fn index(&self, x: RObject<Vector, TF64>) -> &Self::Output {
        &x.slice()[*self]
    }
}

impl Index<RObject<Vector, TI32>> for usize {
    type Output = i32;

    fn index(&self, x: RObject<Vector, TI32>) -> &Self::Output {
        &x.slice()[*self]
    }
}

impl Index<RObject<Matrix, TF64>> for (usize, usize) {
    type Output = f64;

    fn index(&self, x: RObject<Matrix, TF64>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Matrix, TI32>> for (usize, usize) {
    type Output = i32;

    fn index(&self, x: RObject<Matrix, TI32>) -> &Self::Output {
        unimplemented!()
    }
}

impl<const N: usize> Index<RObject<Array, TF64>> for [usize; N] {
    type Output = f64;

    fn index(&self, x: RObject<Array, TF64>) -> &Self::Output {
        unimplemented!()
    }
}

impl<const N: usize> Index<RObject<Array, TI32>> for [usize; N] {
    type Output = i32;

    fn index(&self, x: RObject<Array, TI32>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Vector, Unspecified>> for usize {
    type Output = RObject;

    fn index(&self, x: RObject<Vector, Unspecified>) -> &Self::Output {
        unimplemented!()
    }
}