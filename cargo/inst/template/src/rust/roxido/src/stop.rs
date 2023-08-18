use crate::rbindings::*;

#[doc(hidden)]
pub struct RStopHelper(pub String);

/// Throw an R error.
#[macro_export]
#[allow(clippy::crate_in_macro_def)]
macro_rules! stop {
    () => {
        std::panic::panic_any(crate::RStopHelper(String::new()))
    };
    ($fmt_string:expr) => {
        std::panic::panic_any(crate::RStopHelper(format!($fmt_string)))
    };
    ($fmt_string:expr, $( $arg:expr ),* ) => {
        std::panic::panic_any(crate::RStopHelper(format!($fmt_string, $($arg),*)))
    }
}

pub trait UnwrapOrStop<T> {
    fn stop(self, msg: &str) -> T;
    fn stop_default(self) -> T;
}

impl<T> UnwrapOrStop<T> for Result<T, &str> {
    fn stop(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(_) => stop!("{}", msg),
        }
    }
    fn stop_default(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => stop!("{}", e),
        }
    }
}

#[doc(hidden)]
#[no_mangle]
pub extern "C" fn set_custom_panic_hook() -> SEXP {
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        if panic_info
            .payload()
            .downcast_ref::<crate::RStopHelper>()
            .is_none()
        {
            default_panic(panic_info);
        }
    }));
    unsafe { R_NilValue }
}
