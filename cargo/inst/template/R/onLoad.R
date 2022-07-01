.onLoad <- function(libname, pkgname) {
  okay <- load_library(libname, pkgname)
  assign("_library_okay_", okay, envir=pkg_envir)
  if ( ! okay ) {
    scall <- \(...) stop("Cargo was not found. Please run cargo::install() and then restart R.")
    assign(".Call", scall, envir=pkg_envir)
  }
}

.onAttach <- function(libname, pkgname) {
  if ( ! get("_library_okay_", envir=pkg_envir) ) {
    if ( compile_library(libname, pkgname, FALSE, FALSE) ) {
      packageStartupMessage(sprintf("\nThe %s package is now ready, but you must first restart R.\n\n",pkgname), appendLF=FALSE)
    } else {
      packageStartupMessage(sprintf("\nThe %s package requires Cargo. Please run cargo::install() and then restart R.\n\n",pkgname), appendLF=FALSE)
    }
  }
}

.onUnload <- function(libpath) {
  if ( exists("_library_path_", envir=pkg_envir) ) {
    dyn.unload(get("_library_path_", envir=pkg_envir))
  }
}

.Kall <- function(...) {
  x <- .Call(...)
  if ( inherits(x,"error") ) stop(x) else x
}

pkg_envir <- environment()

#' @importFrom cargo run shlib_get
load_library <- function(libname, pkgname) {
  if ( any(dir.exists(file.path(libname, pkgname, c("libs", "src")))) ) {
    return(TRUE)
  }
  use_cache <- function() {
    path <- cargo::shlib_get(pkgname)
    if ( is.null(path) ) return(FALSE)
    info <- tryCatch(dyn.load(path), error=function(e) e)
    if ( inherits(info, "DLLInfo") ) {
      assign("_library_path_", info[['path']], envir=pkg_envir)
      reg <- getDLLRegisteredRoutines(info, addNames=FALSE)
      lapply(reg[['.Call']], \(f) assign(f$name, f, envir=pkg_envir))
      TRUE
    } else {
      FALSE
    }
  }
  if ( use_cache() ) return(TRUE)
  if ( compile_library(libname, pkgname, TRUE, TRUE) ) use_cache() else FALSE
}

#' @importFrom utils untar
#' @importFrom cargo run shlib_set
compile_library <- function(libname, pkgname, no_prompting, one_time_message=TRUE) {
  if ( one_time_message ) {
    packageStartupMessage("\nCompiling library. This is one-time only. Please be patient.\n\n", appendLF=FALSE)
  }
  temp_dir <- tempdir(check=TRUE)
  target_dir <- tempfile(pattern="rust-", tmpdir=temp_dir)
  dir.create(target_dir, showWarnings=FALSE)
  if ( dir.exists(file.path(libname, pkgname, "rust")) ) {
    file.copy(list.files(file.path(libname, pkgname, "rust"), full.names=TRUE), target_dir, recursive=TRUE)
  } else if ( dir.exists(file.path(libname, pkgname, "inst", "rust")) ) {
    file.copy(list.files(file.path(libname, pkgname, "inst", "rust"), full.names=TRUE), target_dir, recursive=TRUE)
  }
  cwd <- getwd()
  on.exit({
    setwd(cwd)
    unlink(target_dir, recursive=TRUE, force=TRUE, expand=FALSE)
  })
  setwd(target_dir)
  utils::untar("vendor.tar.xz", tar="internal")
  is_windows <- .Platform$OS.type=="windows"
  is_mac <- ( ! is_windows ) && identical(as.vector(Sys.info()["sysname"]),"Darwin")
  rustflags <- if ( is_windows ) {
    c('-C', 'target-cpu=native', '-C', 'link-arg=-L', '-C', sprintf('link-arg=%s', R.home('bin')), '-C', 'link-arg=-lR')
  } else if ( is_mac ) {
    c('-C', 'target-cpu=native', '-Clink-args=-undefined dynamic_lookup')
  } else {
    c('-C', 'target-cpu=native')
  }
  status <- cargo::run("--quiet", "build", "--offline", "--release", minimum_version=file.path(libname, pkgname), rustflags=rustflags, use_packageStartupMessage=TRUE, no_prompting=no_prompting)
  if ( status != 0 ) return(FALSE)
  libname <- if ( is_windows ) "rust.dll" else if ( is_mac ) "librust.dylib" else "librust.so"
  cargo::shlib_set(pkgname, file.path("target","release",libname), FALSE, use_packageStartupMessage=TRUE, no_prompting=no_prompting)
}
