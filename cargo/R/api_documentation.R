#' Browse API Documentation
#'
#' This function opens in a web browser the documentation of the API for the
#' cargo framework.
#'
#' @inheritParams register_calls
#'
#' @return \code{NULL}, invisibly.
#' @export
#'
api_documentation <- function(pkgroot=".") {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(pkgroot)
  setwd("src/rustlib/roxido")
  run("doc","--open",minimum_version="../../../DESCRIPTION")
}
