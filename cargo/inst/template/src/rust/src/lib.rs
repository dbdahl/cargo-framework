mod registration;
use roxido::*;

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

#[roxido]
fn convolve2(a: Rval, b: Rval) -> Rval {
    let (a, xa) = a.coerce_double(pc).unwrap();
    let (b, xb) = b.coerce_double(pc).unwrap();
    let (ab, xab) = Rval::new_vector_double(a.len() + b.len() - 1, pc);
    for xabi in xab.iter_mut() { *xabi = 0.0 }
    for (i, xai) in xa.iter().enumerate() {
        for (j, xbj) in xb.iter().enumerate() {
            xab[i + j] += xai * xbj;
        }
    }
    ab
}

#[roxido]
fn zero(f: Rval, guesses: Rval, stol: Rval, rho: Rval) -> Rval {
    let slice = guesses.slice_double().unwrap();
    let (mut x0, mut x1, tol) = (slice[0], slice[1], stol.as_f64());
    if tol <= 0.0 { panic!("non-positive tol value"); }
    let symbol = Rval::new_symbol("x", pc);
    let mut feval = |x: f64| {
        symbol.assign(rval!(x), rho);
        f.eval(rho, pc).unwrap().as_f64()
    };
    let mut f0 = feval(x0);
    if f0 == 0.0 { return rval!(x0); }
    let f1 = feval(x1);
    if f1 == 0.0 { return rval!(x1); }
    if f0 * f1 > 0.0 { panic!("x[0] and x[1] have the same sign"); }
    loop {
        let xc = 0.5 * (x0 + x1);
        if (x0 - x1).abs() < tol { return rval!(xc); }
        let fc = feval(xc);
        if fc == 0.0 { return rval!(xc); }
        if f0 * fc > 0.0 { x0 = xc; f0 = fc; } else { x1 = xc; }
    }
}
