mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use roxido::*;

#[roxido]
fn convolve2(a: RObject, b: RObject) -> RObject {
    let a = a
        .as_vector()
        .stop_str("'a' not a vector.")
        .to_mode_double(pc)
        .slice();
    let b = b
        .as_vector()
        .stop_str("'b' not a vector.")
        .to_mode_double(pc)
        .slice();
    let r = R::new_vector_double(a.len() + b.len() - 1, pc);
    let ab = r.slice();
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
    let f = f.as_function().stop_str("'f' must be a function.");
    let guesses = guesses
        .as_vector()
        .stop_str("'guesses' must be a vector.")
        .to_mode_double(pc);
    if guesses.len() != 2 {
        stop!("'guesses' must be a vector of length two.");
    }
    let guesses = guesses
        .as_vector()
        .stop_str("'guesses' must be a vector.")
        .as_mode_double()
        .stop_str("'guess' must have storage mode 'double'.")
        .slice();
    if guesses.len() != 2 {
        stop!("'guesses' should be of length two.");
    }
    let (mut x0, mut x1) = (guesses[0], guesses[1]);
    let tol = tol.as_f64().stop_str("'tol' should be a numeric scalar.");
    if !tol.is_finite() || tol <= 0.0 {
        stop!("'tol' must be a strictly positive value.");
    }
    let x_rval = R::new_vector_double(1, pc);
    let x_slice = x_rval.slice();
    let mut g = |x: f64| {
        x_slice[0] = x;
        let Ok(fx) = f.call1(&x_rval, pc) else {
            stop!("Error in function evaluation.");
        };
        let fx = fx
            .as_f64()
            .stop_str("Unexpected return value  from function.");
        if !fx.is_finite() {
            stop!("Non-finite return value from function.");
        }
        fx
    };
    let mut f0 = g(x0);
    if f0 == 0.0 {
        return x0.to_r(pc);
    }
    let f1 = g(x1);
    if f1 == 0.0 {
        return x1.to_r(pc);
    }
    if f0 * f1 > 0.0 {
        stop!("Oops, guesses[0] and guesses[1] have the same sign.");
    }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol {
            return xc.to_r(pc);
        }
        let fc = g(xc);
        if fc == 0.0 {
            return xc.to_r(pc);
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
        vec
    }
}
