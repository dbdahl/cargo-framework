mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use roxido::*;

#[roxido]
fn convolve2(a: RObject, b: RObject) -> RObject {
    let Ok(a) = a.as_vector() else {
        stop!("'a' should be a vector.")
    };
    let Ok(b) = b.as_vector() else {
        stop!("'b' should be a vector.")
    };
    let (_, a) = a.coerce_double(pc);
    let (_, b) = b.coerce_double(pc);
    let (r, ab) = RVector::new_double(a.len() + b.len() - 1, pc);
    for abi in ab.iter_mut() {
        *abi = 0.0;
    }
    for (i, ai) in a.iter().enumerate() {
        for (j, bj) in b.iter().enumerate() {
            ab[i + j] += ai * bj;
        }
    }
    r.as_robject()
}

#[roxido]
fn zero(f: RObject, guesses: RObject, stol: RObject, rho: RObject) -> RObject {
    let Ok(guesses) = guesses.as_vector() else {
        stop!("'guesses' must be a vector.")
    };
    let Ok(slice) = guesses.slice_double() else {
        stop!("'guesses' must have storage mode 'double'.")
    };
    let (mut x0, mut x1, tol) = (slice[0], slice[1], stol.as_f64());
    if tol <= 0.0 {
        stop!("non-positive tol value");
    }
    let symbol = RObject::new_symbol("x", pc);
    let mut feval = |x: f64| {
        let _ = symbol.assign(RObject::new(x, pc), rho);
        f.eval(rho, pc).unwrap().as_f64()
    };
    let mut f0 = feval(x0);
    if f0 == 0.0 {
        return RObject::new(x0, pc);
    }
    let f1 = feval(x1);
    if f1 == 0.0 {
        return RObject::new(x1, pc);
    }
    if f0 * f1 > 0.0 {
        stop!("x[0] and x[1] have the same sign");
    }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol {
            return RObject::new(xc, pc);
        }
        let fc = feval(xc);
        if fc == 0.0 {
            return RObject::new(xc, pc);
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
        let (mean, sd) = (Rf_asReal(mean.as_sexp()), Rf_asReal(sd.as_sexp()));
        let len_i32 = Rf_asInteger(n.as_sexp());
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
        RObject::from_sexp(vec)
    }
}
