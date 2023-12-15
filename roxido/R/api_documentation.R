#' Browse API Documentation
#'
#' This function opens in a web browser the documentation of the Rust API.
#'
#' @return \code{NULL}, invisibly.
#' @export
#'
api_documentation <- function() {
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  find_package_root()
  dir <- file.path("src", "rust", "roxido")
  setwd(dir)
  run("doc", "--open")
  invisible(NULL)
}
