//! The paper
//! [*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
//! complements
//! [*Writing R Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html)
//! (the official guide for writing R extensions) for those interested in developing
//! [R](https://www.r-project.org/) packages using
//! [Rust](https://www.rust-lang.org/). It highlights idiosyncrasies of
//! [R](https://www.r-project.org/) and [Rust](https://www.rust-lang.org/) that must
//! be addressed by any integration and describes how to develop
//! [Rust](https://www.rust-lang.org/)-based packages which comply with the [CRAN
//! Repository Policy](https://cran.r-project.org/web/packages/policies.html).  The
//! [paper]( https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
//! introduces the cargo framework, a
//! transparent [Rust](https://www.rust-lang.org/)-based API which wraps
//! commonly-used parts of [R](https://www.r-project.org/)'s API with minimal
//! overhead and allows a programmer to easily add additional wrappers.
//!
//! This crate provides the Rust API for the cargo framework.  Of particular note
//! is [R], [RObject], [RVector], [RMatrix], [RVectorCharacter], [RList], and [RFunction].

pub mod pc;
pub mod print;
pub mod r;
pub mod rbindings;
pub mod stop;

/// A procedural macro to facilitate calling a Rust function from R.
pub use roxido_macro::roxido;

/// A procedural macro to facilitate printing from Rust to R.
pub use print::*;

#[doc(hidden)]
pub use stop::{RStopHelper, UnwrapOrStop};

pub use pc::Pc;

pub use r::{RObject, ToR1, ToR2, ToR3, ToR4, R};
