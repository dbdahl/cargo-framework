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
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use syn::ext::IdentExt;
use syn::parse::Parser;
use syn::Token;

// See https://doc.rust-lang.org/nomicon/unwinding.html
//
// Which says, in part, "There is an API called catch_unwind that enables catching a panic without spawning a thread. Still, we would encourage you to only do this sparingly. In particular, Rust's current unwinding implementation is heavily optimized for the "doesn't unwind" case. If a program doesn't unwind, there should be no runtime cost for the program being ready to unwind. As a consequence, actually unwinding will be more expensive than in e.g. Java. Don't build your programs to unwind under normal circumstances. Ideally, you should only panic for programming errors or extreme problems."

#[proc_macro_attribute]
pub fn roxido(attr: TokenStream, item: TokenStream) -> TokenStream {
    let options: Vec<_> = syn::punctuated::Punctuated::<NestedMeta, Token![,]>::parse_terminated
        .parse(attr)
        .map(|punctuated| punctuated.into_iter().collect())
        .unwrap();
    match syn::parse_macro_input!(item as syn::Item) {
        syn::Item::Fn(item_fn) => roxido_fn(options, item_fn),
        _ => panic!("The 'roxido' attribute can only be added to a function."),
    }
}

struct NestedMeta(syn::Meta);

impl syn::parse::Parse for NestedMeta {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(syn::Ident::peek_any) {
            input.parse().map(NestedMeta)
        } else {
            Err(input.error("Parse error"))
        }
    }
}

fn roxido_fn(options: Vec<NestedMeta>, item_fn: syn::ItemFn) -> TokenStream {
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
    for meta in options {
        let meta = meta.0;
        let meta_string = quote!(#meta).to_string();
        match meta {
            syn::Meta::NameValue(x) => {
                let name = x.path;
                let value = x.value;
                let name_string = quote!(#name).to_string();
                let value_string = quote!(#value).to_string();
                match name_string.as_str() {
                    "longjmp" => match value_string.as_str() {
                        "true" => longjmp = true,
                        "false" => longjmp = false,
                        _ => {
                            panic!("Unsupported value '{value_string}' for {name_string}.")
                        }
                    },
                    "invisible" => match value_string.as_str() {
                        "true" => invisible = true,
                        "false" => invisible = false,
                        _ => {
                            panic!("Unsupported value '{value_string}' for {name_string}.")
                        }
                    },
                    _ => panic!("Unsupported option '{name_string}'."),
                }
            }
            _ => panic!("Unsupported option '{meta_string}'."),
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
    // Check that all arguments are of type RObject.
    let mut arg_names = Vec::with_capacity(args.len());
    for arg in &args {
        match arg {
            syn::FnArg::Typed(pat_type) => {
                let name = &pat_type.pat;
                arg_names.push(quote!(#name).to_string());
                let ty = &pat_type.ty;
                let string = quote!(#ty).to_string();
                if string != "RObject" {
                    panic!("All arguments to a function with the 'roxido' attribute must be of type RObject, but found '{}'.", string)
                }
            }
            _ => panic!(
                "All arguments to a function with the 'roxido' attribute must be of type RObject."
            ),
        }
    }
    // Check that return is of type RObject.
    match &output {
        syn::ReturnType::Default => panic!(),
        syn::ReturnType::Type(_, tipe) => {
            let tipe_as_string = quote!(#tipe).to_string();
            if tipe_as_string != "RObject" {
                panic!(
                    "A function with the 'roxido' attribute must return RObject, but found '{}'.",
                    tipe_as_string
                );
            }
        }
    }
    let func_name = quote!(#name).to_string();
    if let Some(mut path) = r_function_directory {
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
    } else {
        match std::env::var("R_CARGO_RUN_COUNTER") {
            Ok(x) if x == "1" => {
                let filename = "roxido.txt";
                if let Ok(mut file) = OpenOptions::new().append(true).create(true).open(filename) {
                    let mut line = String::new();
                    line.push_str(&func_name);
                    for arg in arg_names.iter() {
                        line.push_str(", ");
                        line.push_str(arg)
                    }
                    line.push('\n');
                    if file.write_all(line.as_bytes()).is_err() {
                        eprintln!("Couldn't append to file: {filename}");
                    }
                } else {
                    eprintln!("Couldn't open the file: {filename}");
                }
            }
            _ => {}
        }
    }
    // Write the function itself, wrapping the body in 'catch_unwind' to prevent unwinding into C.
    if longjmp {
        // This will long jump, but that's seems to be okay because this is the last Rust stack
        // frame and we've cleaned up all of our heap memory.  Light testing indicated no memory
        // leaks.  See https://docs.rs/crate/setjmp for background information.
        TokenStream::from(quote! {
            #[no_mangle]
            extern "C" fn #name(#args) #output {
                let result: Result<RObject, _> = std::panic::catch_unwind(|| {
                    let pc = &mut Pc::new();
                    #[allow(unused_macros)]
                    macro_rules! rvec { ($val:expr) => { RVector::allocate($val, pc) } }
                    #[allow(unused_macros)]
                    macro_rules! rstr { ($val:expr) => { RVectorCharacter::allocate($val, pc) } }
                    let mut f = || { #body };
                    f().into()
                });
                match result {
                    Ok(obj) => obj,
                    Err(ref payload) => {
                        let mut scratch = String::new();
                        let msg = match payload.downcast_ref::<crate::r::RStopHelper>() {
                            Some(x) => x.0.as_str(),
                            None => {
                                scratch = format!("Panic in Rust function '{}' with 'roxido' attribute.", stringify!(#name));
                                &scratch[..]
                            }
                        };
                        let len = msg.len();
                        let sexp = unsafe {
                            use std::convert::TryInto;
                            crate::rbindings::Rf_mkCharLen(
                                msg.as_ptr() as *const std::os::raw::c_char,
                                msg.len().try_into().unwrap(),
                            )
                        };
                        drop(scratch);
                        drop(result);
                        unsafe {
                            crate::rbindings::Rf_error(b"%.*s\0".as_ptr() as *const std::os::raw::c_char, len, crate::rbindings::R_CHAR(sexp));
                        }
                        crate::RObject::nil() // We never get here.
                    }
                }
            }
        })
    } else {
        TokenStream::from(quote! {
            #[no_mangle]
            extern "C" fn #name(#args) #output {
                let result: Result<RObject,_> = std::panic::catch_unwind(|| {
                    let pc = &mut Pc::new();
                    #[allow(unused_macros)]
                    macro_rules! rvec { ($val:expr) => { RVector::allocate($val, pc) } }
                    #[allow(unused_macros)]
                    macro_rules! rstr { ($val:expr) => { RVectorCharacter::allocate($val, pc) } }
                    let mut f = || { #body };
                    f().into()
                });
                match result {
                    Ok(obj) => obj,
                    Err(_) => {
                        let pc = &mut crate::r::Pc::new();
                        crate::RObject::new_error(format!("Panic in Rust function '{}' with 'roxido' attribute.", stringify!(#name)).as_str(), pc)
                    }
                }
            }
        })
    }
}
