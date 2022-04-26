.onLoad <- function(libname, pkgname) {
    if ( ! load_library(libname, pkgname, TRUE) ) {
        scall <- \(...) stop("Cargo was not found. Did you run cargo::install() and then restart R?")
        assign(".Call", scall, envir=pkg_envir)
    }
}

.onAttach <- function(libname, pkgname) {
    load_library(libname, pkgname, FALSE)
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

#' @importFrom tools R_user_dir
#' @importFrom utils packageVersion
#' @importFrom cargo run
#' @importFrom utils untar
load_library <- function(libname, pkgname, must_be_silent) {
    if ( any(dir.exists(file.path(libname, pkgname, c("libs", "src")))) ) {
        return(TRUE)
    }
    if ( exists("_library_path_", envir=pkg_envir) ) {
        return(TRUE)
    }
    use_cache <- function() {
        path <- cargo::shlib_get(pkgname)
        if ( is.null(path) ) return(FALSE)
        info <- tryCatch(dyn.load(path), error=function(e) e)
        if ( inherits(info, "DLLInfo") ) {
            if ( ! environmentIsLocked(pkg_envir) ) {
                assign("_library_path_", info[['path']], envir=pkg_envir)
                reg <- getDLLRegisteredRoutines(info, addNames=FALSE)
                lapply(reg[['.Call']], \(f) assign(f$name, f, envir=pkg_envir))
            } else {
                if ( ! must_be_silent ) {
                    packageStartupMessage(sprintf("\nThe %s package is now ready. Please restart R and try again.\n\n",pkgname), appendLF=FALSE)
                }
            }
            TRUE
        } else {
            FALSE
        }
    }
    if ( use_cache() ) return(TRUE)
    use_packageStartupMessage <- must_be_silent || local({
        calls <- sys.calls()
        "suppressPackageStartupMessages" %in% sapply(seq_len(sys.nframe()), \(i) as.character(calls[[i]])[1])
    })
    cwd <- getwd()
    temp_dir <- tempdir(check=TRUE)
    target_dir <- tempfile(pattern="rust-", tmpdir=temp_dir)
    dir.create(target_dir, showWarnings=FALSE)
    if ( dir.exists(file.path(libname, pkgname, "rust")) ) {
        file.copy(list.files(file.path(libname, pkgname, "rust"), full.names=TRUE), target_dir, recursive=TRUE)
    } else if ( dir.exists(file.path(libname, pkgname, "inst", "rust")) ) {
        file.copy(list.files(file.path(libname, pkgname, "inst", "rust"), full.names=TRUE), target_dir, recursive=TRUE)
    }
    setwd(target_dir)
    on.exit({
        unlink(target_dir, recursive=TRUE, force=TRUE, expand=FALSE)
        setwd(cwd)
    })
    utils::untar("vendor.tar.gz", tar="internal")
    quiet_args <- if ( use_packageStartupMessage ) "--quiet" else character(0)
    is_windows <- .Platform$OS.type=="windows"
    is_mac <- ( ! is_windows ) && identical(as.vector(Sys.info()["sysname"]),"Darwin")
    rustflags <- if ( is_windows ) {
        c('-C', 'target-cpu=native', '-C', 'link-arg=-L', '-C', sprintf('link-arg=%s', R.home('bin')), '-C', 'link-arg=-lR')
    } else if ( is_mac ) {
        c('-C', 'target-cpu=native', '-Clink-args=-undefined dynamic_lookup')
    } else {
        c('-C', 'target-cpu=native')
    }
    status <- cargo::run(quiet_args, "build", "--offline", "--release", minimum_version=file.path(libname, pkgname), rustflags=rustflags, use_packageStartupMessage=use_packageStartupMessage, must_be_silent=must_be_silent)
    if ( status != 0 ) return(FALSE)
    libname <- if ( is_windows ) "rust.dll" else if ( is_mac ) "librust.dylib" else "librust.so"
    cargo::shlib_set(pkgname, file.path("target","release",libname), FALSE, use_packageStartupMessage, must_be_silent)
    use_cache()
}
