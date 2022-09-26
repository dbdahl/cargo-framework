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
  cache_dir <- cache_dir()
  message <- sprintf('\nThis function will download the Rust toolchain from https://rustup.rs
(an official Rust website) and install it into the directory:
    %s
The directory will then be used to keep the Rust installation up-to-date
and to cache compilation artifacts.  The cargo package purges cache items
every %s days, but you can change the frequency by modifying the last line
of the "%s" file in the directory. You can revoke permission at
any time by deleting the directory.\n\n', cache_dir, days_until_next_purge, basename(last_purge_filename()))
  suggestion <- "Please try again in an interactive session or use 'cargo::install(force=TRUE)'.\n"
  if ( ! get_permission(message, suggestion, force) ) return(invisible(FALSE))
  windows <- .Platform$OS.type=="windows"
  vars <- c(CARGO_HOME= normalizePath(file.path(cache_dir,"cargo"),  mustWork=FALSE),
            RUSTUP_HOME=normalizePath(file.path(cache_dir,"rustup"), mustWork=FALSE))
  if ( unlink(vars, recursive=TRUE, force=TRUE) != 0 ) {
    msg(sprintf("Could not clean out installation directory:\n    %s\nPlease delete this directory.\n",normalizePath(cache_dir)))
    return(invisible(FALSE))
  }
  dir.create(cache_dir, showWarnings=FALSE, recursive=TRUE)
  rustup_init <- file.path(cache_dir, sprintf("rustup-init.%s",ifelse(windows,"exe","sh")))
  URL <- ifelse(windows,"https://win.rustup.rs/x86_64","https://sh.rustup.rs")
  if ( tryCatch(utils::download.file(URL, rustup_init, mode="wb", quiet=FALSE), warning=function(e) 1, error=function(e) 1) != 0 ) {
    msg(sprintf("\nCould not download '%s' to '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n\n", URL, rustup_init))
    return(invisible(FALSE))
  }
  msg("Running installation. Please be patient.\n")
  rustup_init_stdout <- file.path(cache_dir, "rustup-init.stdout")
  rustup_init_stderr <- file.path(cache_dir, "rustup-init.stderr")
  if ( windows ) {
    lines <- paste0(shQuote(normalizePath(rustup_init, mustWork=FALSE)), " --no-modify-path -y --default-host x86_64-pc-windows-gnu")
    rustup_init_bat <- file.path(cache_dir, "rustup-init.bat")
    writeLines(lines, rustup_init_bat)
    status <- system3(rustup_init_bat, stdout=rustup_init_stdout, stderr=rustup_init_stderr, env=vars)
    if ( status != 0 ) {
      msg(sprintf("There was a problem running the rustup installer at '%s'.\nSee '%s' and '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n", rustup_init, rustup_init_stdout, rustup_init_stderr))
      return(invisible(FALSE))
    }
    unlink(rustup_init_bat)
  } else {
    if ( system3("sh", c(shQuote(rustup_init),"--no-modify-path","-y"), stdout=rustup_init_stdout, stderr=rustup_init_stderr, env=vars) != 0 ) {
      msg(sprintf("There was a problem running the rustup installer at '%s'.\nSee '%s' and '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n", rustup_init, rustup_init_stdout, rustup_init_stderr))
      return(invisible(FALSE))
    }
  }
  unlink(rustup_init)
  unlink(rustup_init_stdout)
  unlink(rustup_init_stderr)
  purge_cache(TRUE)
  copy_from_template()
  msg("Installation was successfull.\n\n")
  invisible(TRUE)
}

get_permission <- function(message, suggestion=NULL, force=FALSE) {
  windows <- .Platform$OS.type=="windows"
  if ( isFALSE(force) ) {
    if ( ! interactive() && ! is.null(suggestion) ) {
      msg(suggestion)
      return(FALSE)
    }
    while ( TRUE ) {
      msg(message)
      response <- toupper(trimws(readline("Do you agree? [y/N] ")))
      if ( response %in% c("Y","YES") ) break
      msg("\n")
      if ( response %in% c("N","NO","") ) return(FALSE)
    }
  } else {
    msg(message)
    msg("Agreement accepted due to 'force=TRUE'.\n")
  }
  msg("\n")
  TRUE
}

msg <- function(...) base::message(..., appendLF=FALSE)

