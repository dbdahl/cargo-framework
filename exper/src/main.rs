use std::marker::PhantomData;
use std::ops::Index;

struct AnyType(());
struct Vector(());
struct Matrix(());
struct Array(());
struct Function(());

struct T_f64(());
struct T_i32(());
struct Character(());
struct Unspecified(());

struct Zero(());
struct One(());
struct Two(());
struct Three(());
struct Four(());
struct Five(());
struct Six(());

trait HasLength {}

impl HasLength for Vector {}
impl HasLength for Matrix {}
impl HasLength for Array {}

trait Sliceable {}

impl Sliceable for Vector {}
impl Sliceable for Matrix {}
impl Sliceable for Array {}

trait Numeric {}

impl Numeric for T_f64 {}
impl Numeric for T_i32 {}

struct RObject<RType = AnyType, RMode = Unspecified> {
    ptr: usize,
    rtype: PhantomData<(RType, RMode)>,
}

impl<RType, RMode> RObject<RType, RMode> {

    fn convert<RTypeTo, RModeTo>(self) -> RObject<RTypeTo, RModeTo> {
        RObject {
            ptr: self.ptr,
            rtype: PhantomData,
        }
    }

    pub fn to_base(self) -> RObject {
        self.convert()
    }

    
    pub fn is_double(&self) -> bool {
        unimplemented!();
    }

    pub fn is_integer(&self) -> bool {
        unimplemented!();
    }

    pub fn is_vector(&self) -> bool {
        unimplemented!();
    }

    pub fn is_matrix(&self) -> bool {
        unimplemented!();
    }

    pub fn is_array(&self) -> bool {
        unimplemented!();
    }

    pub fn is_list(&self) -> bool {
        unimplemented!();
    }

    pub fn is_function(&self) -> bool {
        unimplemented!();
    }

    pub fn is_number(&self) -> bool {
        unimplemented!();
    }

    pub fn is_numeric_scalar(&self) -> bool {
        if self.is_number() {
            let len = 0;
            len == 1
        } else {
            false
        }
    }

    pub fn as_f64(&self) -> Result<f64, &'static str> {
        if self.is_numeric_scalar() {
            unimplemented!();
            Ok(1.0)
        } else {
            Err("Canot be interperated as an double scalar")
        }
    }

    pub fn as_i32(&self) -> Result<i32, &'static str> {
        if self.is_numeric_scalar() {
            unimplemented!();
            Ok(1)
        } else {
            Err("Canot be interperated as an integer scalar")
        }
    }

    pub fn as_usize(&self) -> Result<u32, &'static str> {
        if self.is_numeric_scalar() {
            unimplemented!();
            Ok(1)
        } else {
            Err("Canot be interperated as an integer scalar")
        }
    }

    pub fn as_bool(&self) -> Result<bool, &'static str> {
        if self.is_numeric_scalar() {
            unimplemented!();
            Ok(true)
        } else {
            Err("Canot be interperated as an integer scalar")
        }
    }

    pub fn as_vector_double(self) -> Result<RObject<Vector, T_f64>, &'static str> {
        if self.is_vector() && self.is_double() {
            Ok(self.convert())
        } else {
            Err("Not a double vector")
        }
    }

    pub fn as_vector_integer(self) -> Result<RObject<Vector, T_i32>, &'static str> {
        if self.is_vector() && self.is_integer() {
            Ok(self.convert())
        } else {
            Err("Not an integer vector")
        }
    }

    pub fn as_matrix_double(self) -> Result<RObject<Matrix, T_f64>, &'static str> {
        if self.is_matrix() && self.is_double() {
            Ok(self.convert())
        } else {
            Err("Not a double matrix")
        }
    }

    pub fn as_matrix_integer(self) -> Result<RObject<Matrix, T_i32>, &'static str> {
        if self.is_matrix() && self.is_integer() {
            Ok(self.convert())
        } else {
            Err("Not an integer matrix")
        }
    }

    pub fn as_array_double(self) -> Result<RObject<Array, T_f64>, &'static str> {
        if self.is_array() && self.is_double() {
            Ok(self.convert())
        } else {
            Err("Not an array")
        }
    }

    pub fn as_array_integer(self) -> Result<RObject<Array, T_i32>, &'static str> {
        if self.is_array() && self.is_integer() {
            Ok(self.convert())
        } else {
            Err("Not an array")
        }
    }

    pub fn as_list(self) -> Result<RObject<Vector, Unspecified>, &'static str> {
        if self.is_list() {
            Ok(self.convert())
        } else {
            Err("Not a list")
        }
    }

    pub fn as_function0(self) -> Result<RObject<Function, One>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 0 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) one argument")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function1(self) -> Result<RObject<Function, One>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 1 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) one argument")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function2(self) -> Result<RObject<Function, Two>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 2 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) two arguments")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function3(self) -> Result<RObject<Function, Three>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 3 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) three arguments")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function4(self) -> Result<RObject<Function, Four>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 4 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) four arguments")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function5(self) -> Result<RObject<Function, Five>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 5 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) five arguments")
            }
        } else {
            Err("Not a function")
        }
    }

    pub fn as_function6(self) -> Result<RObject<Function, Six>, &'static str> {
        if self.is_function() {
            unimplemented!();
            let n_arguments = 0;
            if n_arguments == 6 {
                Ok(self.convert())
            } else {
                Err("Function does not take (as least) six arguments")
            }
        } else {
            Err("Not a function")
        }
    }
}

impl<S: HasLength, T> RObject<S, T> {
    pub fn length(&self) -> usize {
        unimplemented!()
    }

    pub fn is_scalar(&self) -> bool {
        self.length() == 1
    }

}

impl<S: Sliceable> RObject<S, T_f64> {
    pub fn slice(&self) -> &'static [f64] {
        unimplemented!()
    }

}

impl<S: Sliceable> RObject<S, T_i32> {
    pub fn slice(&self) -> &'static [i32] {
        unimplemented!()
    }

}

impl<T> RObject<Matrix, T> {
    pub fn nrow(&self) -> usize {
        unimplemented!()
    }
    pub fn ncol(&self) -> usize {
        unimplemented!()
    }
}

impl<T> RObject<Array, T> {
    pub fn dim(&self) -> Vec<usize> {
        unimplemented!()
    }
}

impl RObject<Function, Zero> {
    pub fn call<RType, RMode>(&self) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, One> {
    pub fn call<RType, RMode, RType1, RMode1>(
        &self,
        arg1: RObject<RType1, RMode1>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, Two> {
    pub fn call<RType, RMode, RType1, RMode1, RType2, RMode2>(
        &self,
        arg1: RObject<RType1, RMode1>,
        arg2: RObject<RType2, RMode2>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, Three> {
    pub fn call<RType, RMode, RType1, RMode1, RType2, RMode2, RType3, RMode3>(
        &self,
        arg1: RObject<RType1, RMode1>,
        arg2: RObject<RType2, RMode2>,
        arg3: RObject<RType3, RMode3>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, Four> {
    pub fn call<RType, RMode, RType1, RMode1, RType2, RMode2, RType3, RMode3, RType4, RMode4>(
        &self,
        arg1: RObject<RType1, RMode1>,
        arg2: RObject<RType2, RMode2>,
        arg3: RObject<RType3, RMode3>,
        arg4: RObject<RType4, RMode4>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, Five> {
    pub fn call<
        RType,
        RMode,
        RType1,
        RMode1,
        RType2,
        RMode2,
        RType3,
        RMode3,
        RType4,
        RMode4,
        RType5,
        RMode5,
    >(
        &self,
        arg1: RObject<RType1, RMode1>,
        arg2: RObject<RType2, RMode2>,
        arg3: RObject<RType3, RMode3>,
        arg4: RObject<RType4, RMode4>,
        arg5: RObject<RType5, RMode5>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl RObject<Function, Six> {
    pub fn call<
        RType,
        RMode,
        RType1,
        RMode1,
        RType2,
        RMode2,
        RType3,
        RMode3,
        RType4,
        RMode4,
        RType5,
        RMode5,
        RType6,
        RMode6,
    >(
        &self,
        arg1: RObject<RType1, RMode1>,
        arg2: RObject<RType2, RMode2>,
        arg3: RObject<RType3, RMode3>,
        arg4: RObject<RType4, RMode4>,
        arg5: RObject<RType5, RMode5>,
        arg6: RObject<RType6, RMode6>,
    ) -> Result<RObject<RType, RMode>, &'static str> {
        unimplemented!()
    }
}

impl Index<RObject<Vector, T_f64>> for usize {
    type Output = f64;

    fn index(&self, x: RObject<Vector, T_f64>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Vector, T_i32>> for usize {
    type Output = i32;

    fn index(&self, x: RObject<Vector, T_i32>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Matrix, T_f64>> for (usize, usize) {
    type Output = f64;

    fn index(&self, x: RObject<Matrix, T_f64>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Matrix, T_i32>> for (usize, usize) {
    type Output = i32;

    fn index(&self, x: RObject<Matrix, T_i32>) -> &Self::Output {
        unimplemented!()
    }
}

impl<const N: usize> Index<RObject<Array, T_f64>> for [usize; N] {
    type Output = f64;

    fn index(&self, x: RObject<Array, T_f64>) -> &Self::Output {
        unimplemented!()
    }
}

impl<const N: usize> Index<RObject<Array, T_i32>> for [usize; N] {
    type Output = i32;

    fn index(&self, x: RObject<Array, T_i32>) -> &Self::Output {
        unimplemented!()
    }
}

impl Index<RObject<Vector, Unspecified>> for usize {
    type Output = RObject;

    fn index(&self, x: RObject<Vector, Unspecified>) -> &Self::Output {
        unimplemented!()
    }
}

fn main() {
    println!("Hello, world!");
}
