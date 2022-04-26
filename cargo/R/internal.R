# An enhancement of the system2 function which sets environment variables better.
system3 <- function(..., env=character()) {
    if ( length(env) > 0 ) {
        names <- names(env)
        original_env <- sapply(names, function(x) Sys.getenv(x,"<unset>"))
        tmp <- original_env != "<unset>"
        to_restore <- original_env[tmp]
        to_unset <- names(original_env[!tmp])
        tmp <- ! is.na(env)
        if ( sum(tmp) > 0 ) do.call(Sys.setenv, as.list(env[tmp]))
        if ( sum(!tmp) > 0 ) Sys.unsetenv(names(env[!tmp]))
        on.exit({
            if ( length(to_restore) > 0 ) do.call(Sys.setenv, as.list(to_restore))
            Sys.unsetenv(to_unset)
        })
    }
    system2(...)
}

get_homes <- function(cargo_home, rustup_home) {
    c(CARGO_HOME=normalizePath(cargo_home, mustWork=FALSE),
      RUSTUP_HOME=normalizePath(rustup_home, mustWork=FALSE))
}

mk_rustflags <- function(...) {
    args <- c(...)
    if ( is.null(args) ) list()
    else {
        x <- if ( any(is.na(args)) || (length(args) == 0) ) {
            NA
        } else {
            paste(args, collapse=rawToChar(as.raw(strtoi("1f",base=16L))))
        }
        list(CARGO_ENCODED_RUSTFLAGS=x)
    }
}

#' Determine the Rust Build Target
#'
#' This function tries to determine the appropriate Rust target for this
#' instance of R.  Or, it gives the targets necessary for CRAN build machines.
#'
#' @param cran Are targets for all CRAN build machines desired?
#'
#' @return If \code{cran=FALSE}, a string giving a Rust target, or \code{""} if
#'   this cannot be determined.  If \code{cran=TRUE}, a character vector giving
#'   the targets necessary for CRAN build machines.
#'
#' @seealso cross_compile
#'
#' @noRd
#' @examples
#' target()
#'
target <- function(cran=FALSE) {
    if ( isTRUE(cran) ) {
        return(c("i686-pc-windows-gnu","x87_64-pc-windows-gnu","aarch64-apple-darwin","x86_64-apple-darwin","x86_64-unknown-linux-gnu"))
    }
    info <- Sys.info()
    sysname <- info['sysname']
    machine <- info['machine']
    arch <- if ( machine == "x86" ) "i686"
    else if ( grepl("x86[_-]64",machine) ) "x86_64"
    else if ( machine %in% c("aarch64","arm64") ) "aarch64"
    else machine
    result <- if ( .Platform$OS.type == "windows" ) {
        if ( arch %in% c("i686","x86_64") ) paste0(arch,"-pc-windows-gnu")
        else ""
    } else {
        if ( sysname == "Darwin" ) {
            if ( arch %in% c("aarch64","x86_64") ) paste0(arch,"-apple-darwin")
            else ""
        } else if ( sysname == "Linux" ) {
            if ( arch %in% c("aarch64","i686","x86_64") ) paste0(arch,"-unknown-linux-gnu")
            else ""
        } else ""
    }
    if ( result == "" ) {
        cat(sprintf("Unrecognized sysname, machine, architecture, platform, os: %s, %s, %s, %s, %s\n", sysname, machine, arch, R.Version()$platform, R.Version()$os))
    } else {
        cat("Target is: ", result, "\n", sep="")
    }
    result
}
