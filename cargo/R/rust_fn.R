#' Define an R Function Implemented in Rust
#'
#' This function takes Rust code as a string from the last unnamed argument,
#' takes variable names for all other unnamed arguments, compiles the Rust
#' function, and wraps it as an R function.
#'
#' @inheritParams run
#' @param ... Rust code is taken as a string from the last unnamed argument, and
#'   variable names come for all other unnamed arguments.  See example.
#' @param dependencies A character vector of crate dependencies, e.g.,
#'   \code{c('rand = "0.8.5"','rand_pcg = "0.3.1"')}.
#' @param verbose If \code{TRUE}, Cargo prints compilation details.  If
#'   \code{FALSE}, Cargo is run in quiet mode, except for the first time this
#'   function is run.  If \code{"never"}, Cargo is always run in quiet mode. In
#'   any case, errors in code are always shown.
#' @param cached Should Cargo use previously downloaded and compiled artifacts?
#' @param longjmp Should the compiled function use the faster (but experimental)
#'   longjmp functionality when Rust code panics?
#' @param invisible Should the compiled function return values invisibly?
#' @param force If \code{TRUE}, write to cache directory on first usage without
#'   asking for user confirmation.
#' @param which NULL or a vector of length one with name either \code{'tag'} or
#'   \code{'branch'} indicating the specific version of the roxido framework to
#'   use.  If NULL, \code{'c(tag = "latest")'} is used.  This only has an
#'   effect when downloading artifacts (e.g., when \code{cached = FALSE}).
#'
#' @return An R function implemented with the supplied Rust code.
#'
#' @importFrom utils packageDate packageVersion
#' @export
#'
rust_fn <- function(..., dependencies = character(0), minimum_version = "1.31.0", verbose = FALSE, cached = TRUE, longjmp = TRUE, invisible = FALSE, force = FALSE, which = NULL) {
  # Parse arguments
  mc <- match.call(expand.dots = FALSE)
  args <- mc[["..."]]
  if (!is.null(names(args))) {
    nms <- names(args)
    stop(paste0("Unexpected named argument: ", paste0(nms[nms != ""], collapse = ", ")))
  }
  len <- length(args)
  code <- args[[len]]
  args_with_type <- sapply(as.character(args[-len]), function(arg) { if (!grepl(":", arg)) paste0(arg, ": &RObject") else arg })
  all_args <- paste0(args_with_type, collapse = ", ")
  code <- sprintf("#[allow(unused_imports)] use roxido::*; #[roxido(longjmp = %s, invisible = %s)] fn func(%s) { %s\n}", tolower(isTRUE(longjmp)), tolower(isTRUE(invisible)), all_args, paste0(code, collapse = "\n"))
  # Set-up directories
  path_info <- get_lib_path(verbose, cached, force, which)
  if (is.null(path_info)) {
    return(invisible())
  }
  if (path_info[["success"]]) {
    on.exit(add = TRUE, unlink(path_info[["lock"]], recursive = TRUE, force = TRUE))
  }
  path <- path_info[["path"]]
  verbose <- path_info[["verbose"]]
  if (isTRUE(verbose)) cat("Build directory: ", path, "\n", sep = "")
  globals[["lib_counter"]] <- globals[["lib_counter"]] + 1
  libname <- paste0("roxido", globals[["lib_counter"]])
  # Write Cargo.toml file
  rustlib_directory <- file.path(path, "rust")
  cargo_toml_file <- file.path(rustlib_directory, "Cargo.toml")
  toml <- c("[package]", sprintf('name = "%s"', libname), 'version = "0.1.0"', 'edition = "2021"', "")
  toml <- c(toml, "[lib]", 'crate-type = ["cdylib"]', "")
  toml <- c(toml, "[dependencies]", 'roxido = { path="roxido" }', dependencies, "")
  writeLines(toml, cargo_toml_file)
  is_mac <- is_mac()
  is_windows <- is_windows()
  rustflags <- if (is_windows) {
    c("-C", "target-cpu=native", "-C", "link-arg=-L", "-C", sprintf("link-arg=%s", R.home("bin")), "-C", "link-arg=-lR")
  } else if (is_mac) {
    c("-C", "target-cpu=native", "-Clink-args=-undefined dynamic_lookup")
  } else {
    c("-C", "target-cpu=native")
  }
  # Write Rust code
  src_directory <- file.path(rustlib_directory, "src")
  dir.create(src_directory, showWarnings = FALSE)
  lib_rs_filename <- file.path(src_directory, "lib.rs")
  writeLines(code, lib_rs_filename)
  # Clean R source directory
  r_code_directory <- file.path(path, "R")
  unlink(list.files(r_code_directory, full.names = TRUE))
  # Build the shared library
  cwd <- getwd()
  on.exit(add = TRUE, {
    setwd(cwd)
  })
  setwd(rustlib_directory)
  options <- if (!isTRUE(verbose)) "--quiet" else character(0)
  run_result <- run(options, "build", "--release",
    minimum_version = minimum_version, leave_no_trace = FALSE,
    environment_variables = c(ROXIDO_R_FUNC_DIR = r_code_directory), rustflags = rustflags, verbose = isTRUE(verbose)
  )
  if (run_result != 0) {
    stop("Couldn't build Rust code.")
  }
  # Load the shared library
  dynlib.base <- if (!is_windows) paste0("lib", libname) else libname
  dynlib.ext <- if (is_mac) ".dylib" else .Platform$dynlib.ext
  dynlib.filename <- paste0(dynlib.base, dynlib.ext)
  dynlib.name <- paste0(dynlib.base, if (is_mac) dynlib.ext else "")
  dynlib.path <- file.path(rustlib_directory, "target", "release", dynlib.filename)
  dyn.load(dynlib.path)
  .Call("__private_set_custom_panic_hook", PACKAGE = dynlib.name)
  # Source the associated R code
  parent.frame <- new.env(parent = globalenv())
  r_code_files <- list.files(r_code_directory, full.names = TRUE)
  for (r_code_file in r_code_files) {
    name <- basename(r_code_file)
    ptr <- getNativeSymbolInfo(name, dynlib.name)$address
    attr(ptr, "dynlib.path") <- dynlib.path
    reg.finalizer(ptr, function(p) dyn.unload(attr(p, "dynlib.path", TRUE)))
    assign(paste0(".", name), ptr, envir = parent.frame)
    source(r_code_file, local = parent.frame)
  }
  get(name, envir = parent.frame)
}

get_lib_path <- function(verbose, cached, force, which) {
  parent <- cache_dir()
  path <- file.path(parent, "rust_fn")
  if (!dir.exists(path)) {
    message <- sprintf('\nThis function needs to cache files in the directory:
    %s
The cargo package purges cache items every %s days, but you can change
the frequency by modifying the last line of the "%s" file in
the directory.  You can revoke permission at any time by deleting the
directory.\n\n', path, days_until_next_purge, basename(last_purge_filename()))
    if (!get_permission(message, NULL, force)) {
      return(NULL)
    }
    purge_cache(TRUE)
  }
  lock <- paste0(path, ".lock")
  success <- TRUE
  if (!dir.exists(parent)) { # Check if it exists.
    if (!dir.create(parent, showWarnings = FALSE, recursive = TRUE)) { # Try to create it.
      if (!dir.exists(parent)) { # Check if perhaps someone else created, which is fine.
        success <- FALSE
      }
    }
  }
  success <- success && dir.create(lock, showWarnings = FALSE)
  if (!success) {
    warning(sprintf("Could not obtain exclusive lock. Perhaps run:\n    unlink(\"%s\", recursive=TRUE)", lock))
    path <- file.path(tmpdir = tempdir(check = TRUE), paste0("roxido-", Sys.getpid()))
  }
  stamp_file <- file.path(path, "stamp")
  if (!isTRUE(cached) || !file.exists(stamp_file) || packageVersion("cargo") > readRDS(stamp_file)) {
    copy_from_template(which)
  }
  verbose <- if (isTRUE(verbose)) {
    TRUE
  } else if (identical("never", verbose)) {
    FALSE
  } else {
    if (length(list.files(file.path(path, "R"))) == 0) {
      msg("Showing compilation details on first call to 'rust_fn' in this session.\nSuppress this with 'verbose=\"never\"'.\n")
      TRUE
    } else {
      FALSE
    }
  }
  list(path = path, lock = lock, success = success, verbose = verbose)
}

copy_from_template <- function(which = NULL) {
  parent <- cache_dir()
  path <- file.path(parent, "rust_fn")
  unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, showWarnings = FALSE)
  saveRDS(packageVersion("cargo"), file = file.path(path, "stamp"))
  dir.create(file.path(path, "R"), showWarnings = FALSE)
  rustlib_directory <- file.path(path, "rust")
  dir.create(rustlib_directory, showWarnings = FALSE)
  x <- download_roxido_example(which = which)
  on.exit(add = TRUE, {
    unlink(dirname(x), recursive = TRUE, force = TRUE, expand = FALSE)
  })
  file.copy(file.path(x, "src", "rust", "roxido"), rustlib_directory, recursive = TRUE)
  file.copy(file.path(x, "src", "rust", "roxido_macro"), rustlib_directory, recursive = TRUE)
  unlink(file.path(path, "rust", "target"), recursive = TRUE, force = TRUE)
}

#' @importFrom utils download.file untar
download_roxido_example <- function(which = NULL) {
  owner <- "dbdahl"
  base_package_name <- "roxidoExample"
  if (is.null(which)) which <- c(tag = "latest")
  url <- if (identical(names(which), "branch")) {
    sprintf("https://github.com/%s/%s/archive/refs/heads/%s.tar.gz", owner, base_package_name, which)
  } else if (identical(names(which), "tag")) {
    sprintf("https://github.com/%s/%s/archive/refs/tags/%s.tar.gz", owner, base_package_name, which)
  } else {
    stop("'which' argument should be NULL or a vector of length one with name either 'tag' or 'branch'.")
  }
  tarball_filename <- tempfile(sprintf("%s_%s_%s_", owner, base_package_name, which), fileext = ".tar.gz")
  on.exit(add = TRUE, {
    unlink(tarball_filename, recursive = TRUE, force = TRUE, expand = FALSE)
  })
  if (0 != download.file(url, tarball_filename, mode = "wb")) {
    stop("Problem downloading repository from Github.")
  }
  expand_dirname <- tempfile()
  untar(tarball_filename, exdir = expand_dirname)
  original_dirname <- list.files(expand_dirname)
  file.path(expand_dirname, original_dirname)
}

globals <- new.env(parent = emptyenv())
globals[["lib_counter"]] <- 0L

is_mac <- function() identical(as.vector(Sys.info()["sysname"]), "Darwin")
is_windows <- function() identical(.Platform$OS.type, "windows")
