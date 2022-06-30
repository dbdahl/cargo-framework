#' Browse API Documentation
#'
#' This function opens in a web browser the documentation of the API for the
#' Cargo Framework.
#'
#' @inheritParams prebuild
#'
#' @return \code{NULL}, invisibly.
#' @export
#'
api_documentation <- function(pkgroot=".") {
  cwd <- getwd()
  on.exit(setwd(cwd))
  dir <- file.path(pkgroot, "src", "rust", "roxido")
  if ( ! dir.exists(dir) ) {
    stop(sprintf("Oops, '%s' does not appear to be a package root directory.", pkgroot))
  }
  setwd(dir)
  run("doc", "--open")
  invisible(NULL)
}
