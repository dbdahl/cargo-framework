#' Prepare for Building the Package Source
#'
#' This function provides many tools to be used before building an R package
#' based on the Cargo Framework.
#'
#' If a package's Rust code changes a dependency, rerun this function with
#' \code{what=c("contributors","vendor")} to update the files
#' \code{src/rust/vendor.tar.xz}, \code{LICENSE.note}, and \code{inst/AUTHORS}.
#' If a package's usage of [base::.Call()] changes, rerun this function with
#' \code{what="register_calls"} to update the
#' \code{src/rust/src/registration.rs} file. Once you are ready to distribution
#' your package, if you want to allow users to avoid having Cargo installed when
#' installing from source, rerun this function with \code{what="cross_compile"}
#' to update the \sQuote{src/rust/r_cargo_cross_compile.tar} file and deploy its
#' contents to your web server. If you update the \code{roxygen2} documentation,
#' rerun this function to update the \code{*.Rd} files.
#'
#' @param pkgroot The root directory of the package.
#' @param what A character vector indicating the desired action. If it contains
#'   \code{"vendor"}, the Rust dependencies are (re)vendored. If it contains
#'   \code{"contributors"}, the authors and licenses of the Rust dependencies
#'   are extracted. If it contains \code{"register_calls"}, the function
#'   (re)generates Rust code. If it contains \code{"cross_compile"}, the
#'   function cross compiles the static library and creates the file
#'   \sQuote{src/rust/r_cargo_cross_compile.tar}. If it contains
#'   \code{"documentation"}, the function (re)generates documentation. Finally,
#'   if it contains \code{"all"}, all of the previous actions are taken.
#'
#' @return \code{NULL}, invisibly.
#' @importFrom utils tar
#' @export
#'
prebuild <- function(pkgroot=".", what=c("vendor", "contributors", "register_calls", "cross_compile", "documentation", "all")[5]) {
  if ( "all" %in% what ) what <- c("vendor", "contributors", "register_calls", "cross_compile", "documentation")
  pkgroot <- normalizePath(pkgroot)
  description_file <- file.path(pkgroot, "DESCRIPTION")
  r_dir <- file.path(pkgroot, "R")
  src_rust_dir <- file.path(pkgroot, "src", "rust")
  if ( ! file.exists(description_file) || ! dir.exists(r_dir) || ! dir.exists(src_rust_dir) ) {
    stop(sprintf("Oops, '%s' does not appear to be a package root directory.", pkgroot))
  }
  desc <- read.dcf(description_file)
  # vendor
  vendor_engine <- function() {
    original_dir <- getwd()
    on.exit({
      setwd(original_dir)
    })
    setwd(src_rust_dir)
    config_file <- file.path(".cargo", "config.toml")
    if ( file.exists(config_file) ) unlink(config_file, expand=FALSE)
    cargo::run("update")
    dir.create(".cargo", showWarnings=FALSE)
    cargo::run("vendor", stdout=config_file)
    utils::tar("vendor.tar.xz", c("vendor",".cargo"), compression="xz", tar="internal")
  }
  if ( "vendor" %in% what ) vendor_engine()
  # contributors
  contributors_engine <- function() {
    original_dir <- getwd()
    on.exit({
      setwd(original_dir)
    })
    setwd(pkgroot)
    # Authors
    # Requires "cargo install cargo-authors"
    authors_crates <- function() {
      original_dir <- getwd()
      on.exit({
        setwd(original_dir)
      })
      setwd(src_rust_dir)
      x <- cargo::run("authors", "--by-crate", stdout=TRUE)
      if ( ! is.null(attr(x,"status")) && attr(x,"status") != 0 ) {
        stop("Could not run 'cargo authors'. Install it with 'cargo::run(\"install\",\"cargo-authors\")'")
      }
      x
    }
    if ( 'Authors@R' %in% colnames(desc) ) {
      authors_crates <- authors_crates()
      dir.create("inst", showWarnings=FALSE)
      authors_file <- file.path("inst","AUTHORS")
      authors_con <- file(authors_file, open="wt")
      authors_package <- paste0("    ",as.character(eval(parse(text=desc[,'Authors@R']))))
      writeLines(c("The R package authors are:", "", authors_package,'',
                   "The Rust crates upon which this package depends are included in the package",
                   "source in the 'src/rust' directory, most of which are in the archive",
                   "'vendor.tar.xz'.  The names and authors are as follows:",
                   authors_crates[-1]), authors_con)
      close(authors_con)
    }
    license_crates <- function() {
      original_dir <- getwd()
      on.exit({
        setwd(original_dir)
      })
      setwd(src_rust_dir)
      x <- cargo::run("license", "--do-not-bundle", stdout=TRUE)
      if ( ! is.null(attr(x,"status")) && attr(x,"status") != 0 ) {
        stop("Could not run 'cargo license'. Install it with 'cargo::run(\"install\",\"cargo-license\")'")
      }
      x
    }
    # License
    # Requires "cargo install cargo-license"
    license_crates <- license_crates()
    dir.create("inst", showWarnings=FALSE)
    license_file <- file.path("LICENSE.note")
    license_con <- file(license_file, open="wt")
    writeLines(c("The Rust crates upon which this package depends are licensed individually.",
                 "These crates are included in the package source in the 'src/rust' directory,",
                 "most of which are in the archive 'vendor.tar.xz'.  The names, versions, and",
                 "licenses are as follows:", "",
                 license_crates), license_con)
    close(license_con)
  }
  if ( "contributors" %in% what ) contributors_engine()
  # register_class
  if ( "register_calls" %in% what ) {
    register_calls(pkgroot)
  }
  # cross_compile
  cross_compile_engine <- function() {
    original_dir <- getwd()
    on.exit({
      setwd(original_dir)
    })
    setwd(src_rust_dir)
    # R_PLATFORM
    cross_compile_dir <- "r_cargo_cross_compile"
    unlink(cross_compile_dir, recursive=TRUE, force=TRUE, expand=FALSE)
    deploy_dir <- file.path(cross_compile_dir, desc[,"Package"], desc[,"Version"])
    lapply(file.path(deploy_dir, targets()), \(x) dir.create(x, showWarnings=FALSE, recursive=TRUE))
    for ( target in targets() ) {
      if ( cargo::run("build", "--release", "--target", target) != 0 ) {
        stop("Could not build.  Perhaps to you need to add the targets using 'rustup', e.g.,\n    rustup target add ", paste0(targets(),sep=" "))
      }
      file.copy(file.path("target", target, "release", "librust.a"), file.path(deploy_dir, target, "librust.a"), overwrite=TRUE)
    }
    utils::tar(paste0(cross_compile_dir,".tar"), cross_compile_dir, tar="internal")
  }
  if ( "cross_compile" %in% what ) {
    cross_compile_engine()
    if ( Sys.which("just") == "" ) {
      message("Could not find 'just'. Install it with 'cargo::run(\"install\",\"just\")'")
    }
  }
  # documentation
  if ( "documentation" %in% what ) {
    using_roxygen2 <- tryCatch({
      x <- desc[,'RoxygenNote']
      !is.na(x)
    }, error=\(x) FALSE)
    if ( using_roxygen2 && requireNamespace("roxygen2", quietly=TRUE) ) {
      roxygen2::roxygenize(pkgroot)
    }
  }
  invisible(NULL)
}

register_calls <- function(pkgroot=".") {
  map <- hunt_for_calls(pkgroot)
  outfilename <- file.path(pkgroot,"src","rust","src","registration.rs")
  outfile <- file(outfilename, open="w")
  cat(
    "// Generated by the cargo::prebuild() function. Do not edit by hand.

// If usage of .Call() and .Kall() functions in the package's R code changes,
// update this file by rerunning \"cargo::prebuild(DIR)\", where DIR is the root
// directory of this package.

/*
// Below is skeleton code that you can copy to your \"src/rust/src/lib.rs\" file
// and then uncomment. You can freely change the body and arguments names, but
// changing the name of a function or the number of arguments necessitates:
// 1. a corresponding change to invocations of .Call() and .Kall() in the R code
// and 2. rerunning cargo::prebuild().

mod registration;
use roxido::*;
", file=outfile)
  for ( x in names(map) ) {
    longjmp <- if ( map[[x]]$isKall ) "(longjmp = false)" else ""
    cat("\n#[roxido",longjmp,"]\nfn ",x,"(",sep="",file=outfile)
    a <- map[[x]]$args
    for ( i in seq_along(a) ) {
      cat(a[i],": Rval",sep="",file=outfile)
      if ( i != length(a) ) cat(", ",file=outfile)
    }
    cat(") -> Rval {\n    ",file=outfile)
    cat("Rval::nil()\n",file=outfile)
    cat("}\n",file=outfile)
  }
  cat("*/\n\n",file=outfile)
  description_file <- file.path(pkgroot, "DESCRIPTION")
  header <- gsub("YYY", length(map), gsub("XXX", read.dcf(description_file)[,'Package'],
                                          'use roxido::*;

#[no_mangle]
extern "C" fn R_init_XXX_rust(info: *mut rbindings::DllInfo) {
    let mut call_routines = Vec::with_capacity(YYY);
    let mut _names: Vec<std::ffi::CString> = Vec::with_capacity(YYY);
'))
  footer <-
    '    call_routines.push(rbindings::R_CallMethodDef {
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
}'
  entry <-
    '    _names.push(std::ffi::CString::new(".#@_NAME_@#").unwrap());
    call_routines.push(rbindings::R_CallMethodDef {
        name: _names.last().unwrap().as_ptr(),
        fun: unsafe { std::mem::transmute(crate::#@_NAME_@# as *const u8) },
        numArgs: #@_NARGS_@#,
    });
'
  cat(header,file=outfile)
  for ( x in names(map) ) {
    cat(gsub("#@_NARGS_@#",length(map[[x]]$args),gsub("#@_NAME_@#",x,entry)),file=outfile)
  }
  cat(footer,file=outfile)
  cat("\n",file=outfile)
  close(outfile)
  invisible()
}

hunt_for_calls <- function(pkgroot) {
  results <- list()
  engine <- function(x) {
    cl <- class(x)
    if ( cl == "call" ) {
      if ( toString(x[[1]]) %in% c(".Call",".Kall") ) {
        name <- toString(x[[2]])
        if ( (name != "...") && grepl("\\..*", name) ) {
          name <- sub("\\.","",name)
          args <- sapply(x[-(1:2)],toString)
          okay <- grepl("^[a-z,A-Z][a-z,A-Z,0-9,_]*$",args)
          args[!okay] <- paste0("unnamed", seq_len(sum(!okay)))
          y <- list(name=toString(x[[2]]), args=sapply(x[-(1:2)],toString))
          results[[length(results)+1]] <<- list(name=name, args=args, isKall=toString(x[[1]])==".Kall")
        }
      }
      lapply(x[-1], engine)
    } else if ( cl %in% c("<-","{","(","for","if","while","repeat") ) {
      lapply(as.list(x)[-1], engine)
    } else if ( cl %in% c("numeric","character","logical","integer","name","pairlist","srcref","NULL") ) {
      NULL
    } else warning(paste0("Unknown class: ",cl))
  }
  description_file <- file.path(pkgroot, "DESCRIPTION")
  r_dir <- file.path(pkgroot, "R")
  if ( ! file.exists(description_file) || ! dir.exists(r_dir) ) {
    stop(sprintf("Oops, '%s' does not appear to be a package root directory.", pkgroot))
  }
  files <- list.files(r_dir, full.names=TRUE)
  env <- new.env()
  for ( f in files ) source(f, local=env)
  funcs <- mget(ls(env, all.names=TRUE), envir=env)
  map <- list()
  w <- sapply(funcs, is.function)
  if ( length(w) == 0 ) return(map)
  funcs <- funcs[w]
  for ( func in funcs ) {
    engine(body(func))
  }
  for ( r in results ) {
    if ( ! is.null(map[[r$name]]) ) {
      if ( length(map[[r$name]]$args) != length(r$args) ) {
        stop(paste0("Inconsistent number of arguments for function ",r$name))
      }
      pattern <- "^unnamed[0-9]+$"
      if ( sum(grepl(pattern, r$args)) < sum(grepl(pattern, map[[r$name]])) ) {
        map[[r$name]] <- list(args=r$args, isKall=r$isKall)
      }
    } else {
      map[[r$name]] <- list(args=r$args, isKall=r$isKall)
    }
  }
  map
}
