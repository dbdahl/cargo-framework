pub mod r;
pub mod rbindings;

pub use r::{NewProtected, Pc, Rval, TryNewProtected};

/// A procedural macro to facilitate calling a Rust function from R.
pub use roxido_macro::roxido;
