mod registration {
    include!(concat!(env!("OUT_DIR"), "/registration.rs"));
}

use roxido::*;

#[roxido]
fn convolve2(a: Rval, b: Rval) -> Rval {
    // Use `let...else` statements when Rust 1.65 is available.
    let (_, a) = a.coerce_double(pc).unwrap();
    let (_, b) = b.coerce_double(pc).unwrap();
    let (r, ab) = Rval::new_vector_double(a.len() + b.len() - 1, pc);
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
fn zero(f: Rval, guesses: Rval, stol: Rval, rho: Rval) -> Rval {
    // Use `let...else` statements when Rust 1.65 is available.
    let slice = guesses.slice_double().unwrap();
    let (mut x0, mut x1, tol) = (slice[0], slice[1], stol.as_f64());
    if tol <= 0.0 {
        stop!("non-positive tol value");
    }
    let symbol = Rval::new_symbol("x", pc);
    let mut feval = |x: f64| {
        let _ = symbol.assign(rval!(x), rho);
        f.eval(rho, pc).unwrap().as_f64()
    };
    let mut f0 = feval(x0);
    if f0 == 0.0 {
        return rval!(x0);
    }
    let f1 = feval(x1);
    if f1 == 0.0 {
        return rval!(x1);
    }
    if f0 * f1 > 0.0 {
        stop!("x[0] and x[1] have the same sign");
    }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol {
            return rval!(xc);
        }
        let fc = feval(xc);
        if fc == 0.0 {
            return rval!(xc);
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
fn myrnorm(n: Rval, mean: Rval, sd: Rval) -> Rval {
    unsafe {
        use rbindings::*;
        use std::convert::TryFrom;
        let (mean, sd) = (Rf_asReal(mean.0), Rf_asReal(sd.0));
        let len_i32 = Rf_asInteger(n.0);
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
        Rval(vec)
    }
}
