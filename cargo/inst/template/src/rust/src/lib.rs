mod registration;
use roxido::*;

#[roxido]
fn convolve2(a: Rval, b: Rval) -> Rval {
    let Ok((_, a)) = a.coerce_double(pc) else { R::stop("'a' is not a numeric vector."); };
    let Ok((_, b)) = b.coerce_double(pc) else { R::stop("'b' is not a numeric vector."); };
    let (r, ab) = Rval::new_vector_double(a.len() + b.len() - 1, pc);
    for abi in ab.iter_mut() { *abi = 0.0; }
    for (i, ai) in a.iter().enumerate() {
        for (j, bj) in b.iter().enumerate() {
            ab[i + j] += ai * bj;
        }
    }
    r
}

#[roxido]
fn zero(f: Rval, guesses: Rval, stol: Rval, rho: Rval) -> Rval {
    let slice = guesses.slice_double().unwrap();
    let (mut x0, mut x1, tol) = (slice[0], slice[1], stol.as_f64());
    if tol <= 0.0 { R::stop("non-positive tol value"); }
    let symbol = Rval::new_symbol("x", pc);
    let mut feval = |x: f64| {
        symbol.assign(rval!(x), rho);
        f.eval(rho, pc).unwrap().as_f64()
    };
    let mut f0 = feval(x0);
    if f0 == 0.0 { return rval!(x0); }
    let f1 = feval(x1);
    if f1 == 0.0 { return rval!(x1); }
    if f0 * f1 > 0.0 { R::stop("x[0] and x[1] have the same sign"); }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol { return rval!(xc); }
        let fc = feval(xc);
        if fc == 0.0 { return rval!(xc); }
        if f0 * fc > 0.0 { x0 = xc; f0 = fc; } else { x1 = xc; }
    }
}

#[roxido]
fn myrnorm(n: Rval, mean: Rval, sd: Rval) -> Rval {
    unsafe {
        use rbindings::*;
        use std::convert::TryFrom;
        let (mean, sd) = (Rf_asReal(mean.0), Rf_asReal(sd.0));
        let length = isize::try_from(Rf_asInteger(n.0)).unwrap();
        let vec = Rf_protect(Rf_allocVector(REALSXP, length));
        let slice = Rval(vec).slice_mut_double().unwrap();
        GetRNGstate();
        for x in slice { *x = Rf_rnorm(mean, sd); }
        PutRNGstate();
        Rf_unprotect(1);
        Rval(vec)
    }
}

