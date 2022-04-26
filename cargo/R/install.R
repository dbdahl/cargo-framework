#' Install Rust Toolchain
#'
#' This function downloads the \samp{rustup} installer, run it, and adds targets
#' to compile for all the CRAN build machines.
#'
#' @param force If \code{TRUE}, installation proceeds without asking for user
#'   confirmation.
#'
#' @return Invisibly, \code{TRUE} if successful and \code{FALSE} otherwise.
#' @export
#'
#' @export
#'
install <- function(force=FALSE) {
    install_engine(force, FALSE, FALSE)
}

install_engine <- function(force, use_packageStartupMessage, must_be_silent) {
    cat <- function(...) {
        if ( must_be_silent ) return()
        if ( isTRUE(use_packageStartupMessage) ) {
            packageStartupMessage(..., appendLF=FALSE)
        } else {
            base::cat(...)
        }
    }
    cache_dir <- tools::R_user_dir("cargo", "cache")
    days_until_next_purge <- 91
    last_purge_filename <- file.path(cache_dir,"last-purge")
    message <- sprintf(
'\nThe cargo package would like to download Rust from https://rustup.rs/ (an
official website of the Rust project) and then install Rust into the directory:
    %s
That directory will then be used to keep the Rust installation up-to-date. It
will also be used to: 1. cache shared libraries for R packages based on Rust
and 2. enable cached compilation for the cargo::rust_fn function. The cargo
package purges unused cache items every %s days, but you can change the
frequency by modifying the last line of the "%s" file in that
directory. You can revoke permission at any time by deleting that directory.\n\n',
cache_dir, days_until_next_purge, basename(last_purge_filename))
    if ( isFALSE(force) ) {
        if ( must_be_silent || use_packageStartupMessage || ! interactive() ) {
            cat("Please try again in an interactive session or use 'cargo::install(force=TRUE)'.\n")
            return(invisible(FALSE))
        }
        while ( TRUE ) {
            cat(message)
            response <- toupper(trimws(readline(prompt="Do you agree? [y/N] ")))
            if ( response %in% c("N","") ) return(invisible(FALSE))
            if ( response %in% c("Y") ) break
            cat("\n")
        }
        cat("Proceeding with installation. Please be patient.\n")
    } else {
        cat(message)
        cat("Agreement accepted due to 'force=TRUE'.\n")
    }
    cat("\n")
    cargo_home  <- file.path(cache_dir,"cargo")
    rustup_home <- file.path(cache_dir,"rustup")
    vars <- get_homes(cargo_home, rustup_home)
    if ( unlink(vars, recursive=TRUE, force=TRUE) != 0 ) {
        cat(sprintf("Could not clean out installation directory:\n    %s\nPlease delete this directory.\n",normalizePath(cache_dir)))
        return(invisible(FALSE))
    }
    dir.create(cache_dir, showWarnings=FALSE, recursive=TRUE)
    windows <- .Platform$OS.type=="windows"
    rustup_init <- file.path(cache_dir, sprintf("rustup-init.%s",ifelse(windows,"exe","sh")))
    URL <- ifelse(windows,"https://win.rustup.rs/x86_64","https://sh.rustup.rs")
    if ( tryCatch(utils::download.file(URL, rustup_init, mode="wb", quiet=use_packageStartupMessage), warning=function(e) 1, error=function(e) 1) != 0 ) {
        cat(sprintf("Could not download '%s' to '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n", URL, rustup_init))
        return(invisible(FALSE))
    }
    cat("Running installation. Please be patient.\n")
    rustup_init_stdout <- file.path(cache_dir, "rustup-init.stdout")
    rustup_init_stderr <- file.path(cache_dir, "rustup-init.stderr")
    if ( windows ) {
        lines <- paste0(shQuote(normalizePath(rustup_init, mustWork=FALSE)), " --no-modify-path -y --default-host x86_64-pc-windows-gnu")
        rustup_init_bat <- file.path(cache_dir, "rustup-init.bat")
        writeLines(lines, rustup_init_bat)
        status <- system3(rustup_init_bat, stdout=rustup_init_stdout, stderr=rustup_init_stderr, env=vars)
        if ( status != 0 ) {
            cat(sprintf("There was a problem running the rustup installer at '%s'.\nSee '%s' and '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n", rustup_init, rustup_init_stdout, rustup_init_stderr))
            return(invisible(FALSE))
        }
        unlink(rustup_init_bat)
    } else {
        if ( system3("sh", c(shQuote(rustup_init),"--no-modify-path","-y"), stdout=rustup_init_stdout, stderr=rustup_init_stderr, env=vars) != 0 ) {
            cat(sprintf("There was a problem running the rustup installer at '%s'.\nSee '%s' and '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n", rustup_init, rustup_init_stdout, rustup_init_stderr))
            return(invisible(FALSE))
        }
    }
    unlink(rustup_init)
    unlink(rustup_init_stdout)
    unlink(rustup_init_stderr)
    writeLines(c("1",as.character(Sys.Date()),days_until_next_purge), last_purge_filename)
    cat("Installation was successfull.\n")
    invisible(TRUE)
}
