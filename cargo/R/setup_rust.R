#' Setup Rust Toolchain
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
setup_rust <- function(force=FALSE) {
  windows <- .Platform$OS.type=="windows"
  if ( ! force ) {
    if ( ! interactive() ) stop("This function requires either user confirmation or 'force=TRUE'.")
      while ( TRUE ) {
        response <- toupper(trimws(readline(prompt=paste0("Do you want to install the Rust toolchain and targets? [y/N] "))))
        if ( response %in% c("N","") ) return(invisible(FALSE))
        if ( response %in% c("Y") ) break
      }
  }
  temp_install_home <- mkdir(tempfile("cargo"))
  rustup_init <- file.path(temp_install_home, sprintf("rustup-init.%s",ifelse(windows,"exe","sh")))
  URL <- ifelse(windows,"https://win.rustup.rs/x86_64","https://sh.rustup.rs")
  if ( tryCatch(utils::download.file(URL, rustup_init, mode="wb"), warning=function(e) 1, error=function(e) 1) != 0 ) {
    cat(sprintf("Could not download '%s' to '%s'.\n", URL, rustup_init))
    return(invisible(FALSE))
  }
  if ( windows ) {
    lines <- paste0(shQuote(normalizePath(rustup_init,mustWork=FALSE))," -y --default-host x86_64-pc-windows-gnu")
    rustup_init_bat <- file.path(temp_install_home, "rustup-init.bat")
    writeLines(lines, rustup_init_bat)
    if ( system2(rustup_init_bat) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init_bat))
      return(invisible(FALSE))
    }
  } else {
    if ( system2("sh", c(shQuote(rustup_init),"-y")) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init))
      return(invisible(FALSE))
    }
  }
  unlink(temp_install_home, recursive=TRUE, force=TRUE)
  n <- function(x) normalizePath(x, mustWork=FALSE)
  rustup_cmd <- n(find_cmd("rustup"))
  for ( target in target(cran=TRUE) ) {
    if ( system2(rustup_cmd,c("target","add",target)) != 0 ) {
      cat(sprintf("There was a problem running rustup at '%s'.\n", rustup_cmd))
      return(invisible(FALSE))
    }
  }
  cat("\n### Rust was successfully set up. ###\n\n")
  invisible(TRUE)
}
