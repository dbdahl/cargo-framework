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
  if (!find_package_root()) {
    parent <- cache_dir()
    dir <- file.path(parent, "rust_fn", "rust", "roxido")
    if (dir.exists(dir)) {
      setwd(dir)
    } else {
      stop("Cannot find package root.")
    }
  }
  run("doc", "--open")
  invisible(NULL)
}
