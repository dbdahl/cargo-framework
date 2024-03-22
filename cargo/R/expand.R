#' Run Cargo Expand for R Packages
#'
#' The 'roxido' framework uses macros and this function will expand all macros and display to source code.
#'
#' @return \code{NULL}, invisibly.
#' @export
#'
expand <- function() {
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  if (!find_package_root()) {
    stop("Cannot find package root.")
  }
  desc <- read.dcf("DESCRIPTION")
  pkgname <- desc[, "Package"]
  src_rust_dir <- file.path("src", "rust")
  setwd(src_rust_dir)
  # Requires "cargo install cargo-expand"
  run("build", environment_variables = list(R_PACKAGE_NAME = pkgname), run_twice = TRUE)
  x <- run("expand", environment_variables = list(R_PACKAGE_NAME = pkgname, R_CARGO_RUN_COUNTER = "2"))
  if (!is.null(attr(x, "status")) && attr(x, "status") != 0) {
    stop("Could not run 'cargo expand'. Install it with 'cargo::run(\"install\",\"cargo-expand\")'")
  }
  invisible(NULL)
}
