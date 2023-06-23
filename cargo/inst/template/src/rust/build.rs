use std::env;
use std::fs;
use std::path::Path;

fn main() {
    if let Some(out_dir) = env::var_os("OUT_DIR") {
        let snippet = {
            let src_path = Path::new("roxido.txt");
            match env::var("R_PACKAGE_NAME") {
                Err(_) => String::new(),
                Ok(package_name) => match env::var("R_CARGO_SECOND_RUN") {
                    Err(_) => {
                        let _ = fs::remove_file(src_path);
                        String::new()
                    }
                    Ok(_) => {
                        let functions_info =
                            fs::read_to_string(src_path).expect("Could not read file: {src_path}");
                        let mut functions_info: Vec<_> = functions_info.lines().collect();
                        functions_info.sort_unstable();
                        functions_info.dedup();
                        let mut snippet = String::new();
                        snippet.push_str(&format!(
                            r#"
                            use roxido::*;

                            #[no_mangle]
                            extern "C" fn R_init_{}_rust(info: *mut rbindings::DllInfo) {{
                                let mut call_routines = Vec::with_capacity({});
                                let mut _names: Vec<std::ffi::CString> = Vec::with_capacity({});
                            "#,
                            package_name,
                            functions_info.len(),
                            functions_info.len()
                        ));
                        for line in functions_info {
                            let tidbits: Vec<_> = line.split_whitespace().collect();
                            snippet.push_str(&format!(
                                r#"
                                _names.push(std::ffi::CString::new(".{}").unwrap());
                                call_routines.push(rbindings::R_CallMethodDef {{
                                    name: _names.last().unwrap().as_ptr(),
                                    fun: unsafe {{ std::mem::transmute(crate::{} as *const u8) }},
                                    numArgs: {},
                                }});"#,
                                tidbits[0], tidbits[0], tidbits[1]
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
                        snippet
                    }
                },
            }
        };
        let dest_path = Path::new(&out_dir).join("registration.rs");
        fs::write(&dest_path, snippet).expect("Could not write file: {dest_path}");
    }
}
