use std::env;
use std::fs;
use std::path::Path;

fn main() {
    println!(
        "Running build.rs with working directory {:?}",
        std::env::current_dir()
    );
    println!("cargo:rerun-if-env-changed=R_CARGO_RUN_COUNTER");
    let src_path = Path::new("roxido.txt");
    let snippet = match env::var("R_CARGO_RUN_COUNTER") {
        Ok(x) if x == "1" => None,
        Ok(x) if x == "2" => make_registration_code(src_path),
        Ok(_) => {
            println!("Environment variable 'R_CARGO_RUN_COUNTER' has an unexpected value.");
            None
        }
        Err(_) => {
            println!("Environment variable 'R_CARGO_RUN_COUNTER' is not set.");
            None
        }
    };
    let out_dir = env::var_os("OUT_DIR").expect(
        "Environment variable 'OUT_DIR' is not set.  Are you run as part of 'cargo build'?",
    );
    let dest_path = Path::new(&out_dir).join("registration.rs");
    let snippet = snippet.unwrap_or_else(|| {
        let _ = fs::remove_file(src_path);
        String::new()
    });
    fs::write(dest_path, snippet).expect("Could not write file: {dest_path}");
}

fn make_registration_code(src_path: &Path) -> Option<String> {
    match env::var("R_PACKAGE_NAME") {
        Err(_) => None,
        Ok(package_name) => match fs::read_to_string(src_path) {
            Err(_) => None,
            Ok(functions_info) => {
                let mut buffer = String::new();
                buffer.push_str("// Automatically regenerated. Do not edit.\n\n");
                buffer.push_str("void R_init_");
                buffer.push_str(&package_name);
                buffer.push_str("_rust(void *dll); \n");
                buffer.push_str("void R_init_");
                buffer.push_str(&package_name);
                buffer.push_str("(void *dll) { R_init_");
                buffer.push_str(&package_name);
                buffer.push_str("_rust(dll); }\n");
                let _ = fs::write("../shim.c", &buffer);
                let mut functions_info: Vec<_> = functions_info.lines().collect();
                functions_info.sort_unstable();
                functions_info.dedup();
                buffer.clear();
                buffer.push_str("# Automatically regenerated. Do not edit.\n\n");
                for &line in functions_info.iter() {
                    buffer.push_str("# .Call(.");
                    buffer.push_str(line);
                    buffer.push_str(")\n");
                }
                buffer.push_str(
                    r#"
#' @docType package
#' @usage NULL
#' @useDynLib "#,
                );
                buffer.push_str(&package_name);
                buffer.push_str(
                    r#", .registration = TRUE
NULL

.Kall <- function(...) {
  x <- .Call(...)
  if (inherits(x, "error")) stop(x) else x
}"#,
                );
                let _ = fs::write("../../R/roxido.R", &buffer);
                let mut snippet = String::new();
                snippet.push_str(&format!(
                    r#"use roxido::*;

#[no_mangle]
extern "C" fn R_init_{}_rust(info: *mut rbindings::DllInfo) {{
    let mut call_routines = Vec::with_capacity({});
    let mut _names: Vec<std::ffi::CString> = Vec::with_capacity({});"#,
                    package_name,
                    functions_info.len(),
                    functions_info.len()
                ));
                for line in functions_info {
                    let tidbits: Vec<_> = line.split_whitespace().collect();
                    let func_name_with_comma = tidbits[0];
                    let (func_name, should_be_comma) =
                        func_name_with_comma.split_at(func_name_with_comma.len() - 1);
                    let func_name = if should_be_comma.is_empty() || should_be_comma != "," {
                        func_name_with_comma
                    } else {
                        func_name
                    };
                    let n_args = tidbits.len() - 1;
                    snippet.push_str(&format!(
                        r#"
    _names.push(std::ffi::CString::new(".{}").unwrap());
    call_routines.push(rbindings::R_CallMethodDef {{
        name: _names.last().unwrap().as_ptr(),
        fun: unsafe {{ std::mem::transmute(crate::{} as *const u8) }},
        numArgs: {},
    }});"#,
                        func_name, func_name, n_args
                    ));
                }
                snippet.push_str(
                    r#"
    call_routines.push(rbindings::R_CallMethodDef {
        name: std::ptr::null(),
        fun: None,
        numArgs: 0,
    });
    unsafe {
        rbindings::R_registerRoutines(
            info,
            std::ptr::null(),
            call_routines.as_ptr(),
            std::ptr::null(),
            std::ptr::null(),
        );
        rbindings::R_useDynamicSymbols(info, 0);
        rbindings::R_forceSymbols(info, 1);
    }
    roxido::r::set_custom_panic_hook();
}"#,
                );
                Some(snippet)
            }
        },
    }
}
