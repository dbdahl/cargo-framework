mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use roxido::*;

#[roxido]
fn convolve2(a: RObject, b: RObject) -> RObject {
    let a = a.as_vector_or_stop("'a' not a vector.").coerce_double(pc).1;
    let b = b.as_vector_or_stop("'b' not a vector.").coerce_double(pc).1;
    let (r, ab) = RVector::new_double(a.len() + b.len() - 1, pc);
    for abi in ab.iter_mut() {
        *abi = 0.0;
    }
    for (i, ai) in a.iter().enumerate() {
        for (j, bj) in b.iter().enumerate() {
            ab[i + j] += ai * bj;
        }
    }
    r
}

#[roxido]
fn zero(f: RObject, guesses: RObject, tol: RObject) -> RObject {
    let f = f.as_function_or_stop("'f' must be a function.");
    let guesses = guesses.as_vector_or_stop("'guesses' must be a vector.");
    if guesses.len() != 2 {
        stop!("'guesses' must be a vector of length two.")
    }
    let Ok(guesses) = guesses.slice_double() else {
        stop!("'guesses' must have storage mode 'double'.")
    };
    let (mut x0, mut x1, tol) = (guesses[0], guesses[1], tol.as_f64());
    if !tol.is_finite() || tol <= 0.0 {
        stop!("'tol' must be a strictly positive value.");
    }
    let (x_rval, x_slice) = RVector::new_double(1, pc);
    let mut g = |x: f64| {
        x_slice[0] = x;
        let Ok(fx) = f.call1(x_rval, pc) else {
            stop!("Error in function evaluation.")
        };
        let fx = fx.as_f64();
        if !fx.is_finite() {
            stop!("Non-finite function evaluation.")
        }
        fx
    };
    let mut f0 = g(x0);
    if f0 == 0.0 {
        return rvec!(x0);
    }
    let f1 = g(x1);
    if f1 == 0.0 {
        return rvec!(x1);
    }
    if f0 * f1 > 0.0 {
        stop!("Oops, guesses[0] and guesses[1] have the same sign.");
    }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol {
            return rvec!(xc);
        }
        let fc = g(xc);
        if fc == 0.0 {
            return rvec!(xc);
        }
        if f0 * fc > 0.0 {
            x0 = xc;
            f0 = fc;
        } else {
            x1 = xc;
        }
    }
}

#[roxido]
fn myrnorm(n: RObject, mean: RObject, sd: RObject) -> RObject {
    unsafe {
        use rbindings::*;
        use std::convert::TryFrom;
        let (mean, sd) = (Rf_asReal(*mean), Rf_asReal(*sd));
        let len_i32 = Rf_asInteger(*n);
        let len_isize = isize::try_from(len_i32).unwrap();
        let len_usize = usize::try_from(len_i32).unwrap();
        let vec = Rf_protect(Rf_allocVector(REALSXP, len_isize));
        let slice = std::slice::from_raw_parts_mut(REAL(vec), len_usize);
        GetRNGstate();
        for x in slice {
            *x = Rf_rnorm(mean, sd);
        }
        PutRNGstate();
        Rf_unprotect(1);
        RObject(vec)
    }
}
