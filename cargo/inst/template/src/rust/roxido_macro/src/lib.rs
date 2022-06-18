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
//! This crate provides the `roxido` procedural macro to support the
//! [roxido](https://crates.io/crates/roxido) crate

use proc_macro::TokenStream;
use quote::quote;
use std::fs::File;
use std::io::Write;
use std::path::Path;

// See https://doc.rust-lang.org/nomicon/unwinding.html
//
// Which says, in part, "There is an API called catch_unwind that enables catching a panic without spawning a thread. Still, we would encourage you to only do this sparingly. In particular, Rust's current unwinding implementation is heavily optimized for the "doesn't unwind" case. If a program doesn't unwind, there should be no runtime cost for the program being ready to unwind. As a consequence, actually unwinding will be more expensive than in e.g. Java. Don't build your programs to unwind under normal circumstances. Ideally, you should only panic for programming errors or extreme problems."

#[proc_macro_attribute]
pub fn roxido(attr: TokenStream, item: TokenStream) -> TokenStream {
    let options = syn::parse_macro_input!(attr as syn::AttributeArgs);
    match syn::parse_macro_input!(item as syn::Item) {
        syn::Item::Fn(item_fn) => roxido_fn(options, item_fn),
        _ => panic!("The 'roxido' attribute can only be added to a function."),
    }
}

fn roxido_fn(options: Vec<syn::NestedMeta>, item_fn: syn::ItemFn) -> TokenStream {
    let r_function_directory = match std::env::var("ROXIDO_R_FUNC_DIR") {
        Ok(x) if !x.is_empty() => {
            let path = Path::new(&x).to_owned();
            if path.exists() && path.is_dir() {
                Some(path)
            } else {
                None
            }
        }
        _ => None,
    };
    let mut longjmp = true;
    let mut invisible = false;
    for option in options {
        let option_string = quote!(#option).to_string();
        match option {
            syn::NestedMeta::Meta(syn::Meta::NameValue(name_value)) => {
                let path = name_value.path;
                let name = quote!(#path).to_string();
                match name.as_str() {
                    "longjmp" => match name_value.lit {
                        syn::Lit::Bool(x) => longjmp = x.value,
                        _ => panic!("Unsupported option '{}'.", option_string),
                    },
                    "invisible" => match name_value.lit {
                        syn::Lit::Bool(x) => invisible = x.value,
                        _ => panic!("Unsupported option '{}'.", option_string),
                    },
                    _ => panic!("Unsupported option '{}'.", option_string),
                }
            }
            _ => panic!("Unsupported option '{}'.", option_string),
        }
    }
    let name = item_fn.sig.ident;
    let vis = item_fn.vis;
    let args = item_fn.sig.inputs;
    let body = item_fn.block;
    let output = item_fn.sig.output;
    // Check that visibility is okay.
    let vis_as_string = quote!(#vis).to_string();
    if !vis_as_string.is_empty() {
        panic!("A function with the 'roxido' attribute must not have a visibility modifier, but found '{}'.", vis_as_string);
    }
    // Check that all arguments are of type Rval.
    let mut arg_names = Vec::new();
    for arg in &args {
        match arg {
            syn::FnArg::Typed(pat_type) => {
                let name = &pat_type.pat;
                arg_names.push(quote!(#name).to_string());
                let ty = &pat_type.ty;
                let string = quote!(#ty).to_string();
                if string != "Rval" {
                    panic!("All arguments to a function with the 'roxido' attribute must be of type Rval, but found '{}'.", string)
                }
            }
            _ => panic!(
                "All arguments to a function with the 'roxido' attribute must be of type Rval."
            ),
        }
    }
    // Check that return is of type Rval.
    match &output {
        syn::ReturnType::Default => panic!(),
        syn::ReturnType::Type(_, tipe) => {
            let tipe_as_string = quote!(#tipe).to_string();
            if tipe_as_string != "Rval" {
                panic!(
                    "A function with the 'roxido' attribute must return Rval, but found '{}'.",
                    tipe_as_string
                );
            }
        }
    }
    if let Some(mut path) = r_function_directory {
        let func_name = quote!(#name).to_string();
        path.push(&func_name);
        let mut file = File::create(&path)
            .unwrap_or_else(|_| panic!("Could not open file '{:?}' for writing.", &path));
        let args = arg_names.join(", ");
        let comma = if args.is_empty() { "" } else { ", " };
        let (invisible_opening, invisible_closing) = match invisible {
            true => ("invisible(", ")"),
            false => ("", ""),
        };
        if longjmp {
            write!(
                file,
                "{} <- function({}) {}.Call(.{}{}{}){}",
                func_name, args, invisible_opening, func_name, comma, args, invisible_closing
            )
        } else {
            write!(
                file,
                "{} <- function({}) {{\n  x <- .Call(.{}{}{})\n  if ( inherits(x,'error') ) stop(x) else {}x{}\n}}",
                func_name, args, func_name, comma, args, invisible_opening, invisible_closing
            )
        }
        .expect("Could not write to the file.");
    }
    // Write the function itself, wrapping the body in 'catch_unwind' to prevent unwinding into C.
    if longjmp {
        // This will long jump, but that's seems to be okay because this is the last Rust stack
        // frame and we've cleaned up all of our heap memory.  Light testing indicated no memory
        // leaks.  See https://docs.rs/crate/setjmp for background information.
        TokenStream::from(quote! {
            #[no_mangle]
            extern "C" fn #name(#args) #output {
                let result: Result<Rval,_> = std::panic::catch_unwind(|| {
                    let pc = &mut Pc::new();
                    #body
                });
                match result {
                    Ok(obj) => obj,
                    Err(_) => {
                        let msg = format!("Panic in Rust function '{}' with 'roxido' attribute.", stringify!(#name));
                        let str = msg.as_str();
                        let len = str.len();
                        let sexp = unsafe {
                            use std::convert::TryInto;
                            crate::rbindings::Rf_mkCharLen(
                                str.as_ptr() as *const std::os::raw::c_char,
                                str.len().try_into().unwrap(),
                            )
                        };
                        drop(msg);
                        drop(result);
                        unsafe {
                            crate::rbindings::Rf_error(b"%.*s\0".as_ptr() as *const std::os::raw::c_char, len, crate::rbindings::R_CHAR(sexp));
                        }
                        crate::Rval::nil() // We never get here.
                    }
                }
            }
        })
    } else {
        TokenStream::from(quote! {
            #[no_mangle]
            extern "C" fn #name(#args) #output {
                let result: Result<Rval,_> = std::panic::catch_unwind(|| {
                    let pc = &mut Pc::new();
                    #body
                });
                match result {
                    Ok(obj) => obj,
                    Err(_) => {
                        let pc = &mut crate::r::Pc::new();
                        crate::Rval::new_error(format!("Panic in Rust function '{}' with 'roxido' attribute.", stringify!(#name)).as_str(), pc)
                    }
                }
            }
        })
    }
}
