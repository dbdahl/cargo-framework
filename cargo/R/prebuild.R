#' Prepare for Building the Package Source
#'
#' This function provides many tools to be used before building an R package
#' based on the Cargo Framework.
#'
#' If a package's usage of [base::.Call()] changes, rerun this function with
#' \code{what="register_calls"} to update the
#' \code{src/rust/src/registration.rs} file. If you update the \code{roxygen2}
#' documentation, rerun this function with \code{what="document"} to update the
#' \code{*.Rd} files. If a package's Rust code changes a dependency, rerun this
#' function with \code{what=c("authors","vendor")} to update the files
#' \code{src/rust/vendor.tar.xz} and to generate the file
#' \code{authors-scratch.txt} (which will need to be manually incorporated into
#' the \code{DESCRIPTION} file and then deleted). To perform all of these
#' actions, use \code{what="all"}.
#'
#' @param what A character vector indicating the desired action.
#' @param pkgroot The root directory of the package.
#'
#' @return \code{NULL}, invisibly.
#' @importFrom utils tar as.person
#' @export
#'
prebuild <- function(what=c("register_calls", "document", "vendor", "authors", "all")[5], pkgroot=".") {
  if ( "all" %in% what ) what <- c("register_calls", "document", "vendor", "authors")
  if ( !all(what %in% c("register_calls", "document", "vendor", "authors"))) {
    stop("Unrecognized value for 'what' argument.")
  }
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
  # authors
  authors_engine <- function() {
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
      x <- cargo::run("authors", stdout=TRUE)
      if ( ! is.null(attr(x,"status")) && attr(x,"status") != 0 ) {
        stop("Could not run 'cargo authors'. Install it with 'cargo::run(\"install\",\"cargo-authors\")'")
      }
      x[-(1:2)]
    }
    if ( 'Authors@R' %in% colnames(desc) ) {
      authors_crates <- authors_crates()
      authors_crates <- unlist(lapply(strsplit(authors_crates, ":"), \(x) { x <- trimws(x); y <- list(strsplit(x[2],", ")[[1]]); names(y) <- x[1]; y }), recursive=FALSE)
      crates <- cbind(names(authors_crates), sapply(authors_crates, \(x) {
        ss <- if ( length(x) > 1 ) "s" else ""
        sprintf(', role = "cph", comment = "Rust crate%s: %s. See AUTHORS file.")', ss, paste(x,collapse=", "))
      }))
      authors_file <- file.path("authors-scratch.txt")
      authors_con <- file(authors_file, open="wt")
      g <- function(x) {
        if ( is.null(x) || ( length(x) == 0 ) ) '""'
        else if ( length(x) == 1 ) paste0('"',x,'"')
        else {
          paste0('"',paste(x,collapse=' '),'"')
        }
      }
      g <- function(x) paste(gsub('"','\\\\"',x),collapse=" ")
      writeLines("Authors@R: c(", authors_con)
      for ( i in seq_len(nrow(crates)) ) {
        x <- crates[i,]
        p <- as.person(x[1])
        str <- x[2]
        if ( ! is.null(p$email) ) str <- sprintf(', email = "%s"%s',p$email,str)
        gg <- g(p$given)
        gf <- g(p$family)
        str <- if ( is.null(p$given) && is.null(p$family) ) {
          stop("Both given and family names are null.")
        } else if ( is.null(p$given) ) {
          sprintf('person(family = "%s"%s',gf,str)
        } else if ( is.null(p$family) ) {
          sprintf('person(given = "%s"%s',gg,str)
        } else if ( ( tolower(p$given[1]) == "the") || ( tolower(p$family[1]) %in% c("contributors","developers") ) ) {
          sprintf('person(given = "%s"%s',paste0(gg," ",gf),str)
        } else {
          sprintf('person(given = "%s", family = "%s"%s',gg,gf,str)
        }
        cc <- if ( i != nrow(crates) ) "," else ""
        writeLines(paste0("             ",str,cc), authors_con)
      }
      writeLines("            )", authors_con)
      if (all(Sys.which(c("find","xargs","sort","uniq"))!="")) {
        others <- system("find src/rust/vendor -type f -print0 | xargs -0 grep Copyright | sort | uniq", intern=TRUE)
        writeLines("\n\nOther things to consider...", authors_con)
        writeLines(others, authors_con)
      }
      close(authors_con)
    }
  }
  if ( "authors" %in% what ) authors_engine()
  # register_class
  if ( "register_calls" %in% what ) {
    register_calls(pkgroot)
  }
  # document
  if ( "document" %in% what ) {
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
