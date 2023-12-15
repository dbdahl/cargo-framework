use crate::rbindings::*;

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
