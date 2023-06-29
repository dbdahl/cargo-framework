#' Build a Source Package for Submission to CRAN
#'
#' This function builds a source package in preparation for submission to
#' [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).
#' and saved it in the root of a package.
#' In particular, Rust crates upon which the package depends are \dQuote{vendored}
#' within the source package in the archive file \code{src/rust/vendor.tar.xz}, so
#' that lacking internet access will not give a check warning nor error on CRAN.
#' The package's \code{configure} script tests for the existence of this archive
#' file and, when present, runs Cargo (Rust's package manager) in compliance with the
#' [CRAN Repository Policies](https://cran.r-project.org/web/packages/policies.html)
#' in that Cargo will only use two CPU cores and will clean-up cached values (i.e.,
#' remove detritus).
#'
#' Since depending crates are vendored, the authorship and copyright must be
#' declared in the \code{DESCRIPTION} file prior to building the source package
#' for CRAN. See the \code{\link{authors}} function for help in attribution.
#'
#' This function will rebuild \pkg{roxygen2} documentation if the DESCRIPTION
#' file indicates that \pkg{roxygen2} is used and the package is installed.
#'
#' This function does not test the package. The developer is strongly encouraged
#' to both inspect and test the package before submitting to CRAN.
#'
#' @param ... Options passed to \code{R CMD build}.
#'
#' @return The exit status code{R CMD build}, invisibly.
#' @importFrom utils tar
#' @export
#'
build_for_cran <- function(...) {
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  desc <- find_package_root()
  using_roxygen2 <- tryCatch(
    {
      x <- desc[, "RoxygenNote"]
      !is.na(x)
    },
    error = \(x) FALSE
  )
  if (using_roxygen2 && requireNamespace("roxygen2", quietly = TRUE)) {
    roxygen2::roxygenize()
  }
  src_rust_dir <- file.path("src", "rust")
  setwd(src_rust_dir)
  config_file <- file.path(".cargo", "config.toml")
  if (file.exists(config_file)) unlink(config_file, expand = FALSE)
  run("update")
  dir.create(".cargo", showWarnings = FALSE)
  run("vendor", stdout = config_file)
  utils::tar("vendor.tar.xz", c("vendor", ".cargo"), compression = "xz", tar = "internal")
  setwd(file.path("..", ".."))
  cmd0 <- file.path(R.home("bin"), "R")
  args0 <- c("CMD", "build", ..., ".")
  status <- system2(cmd0, args0)
  setwd(src_rust_dir)
  unlink(config_file, expand = FALSE)
  unlink("vendor.tar.xz", expand = FALSE)
  unlink("vendor", recursive = TRUE, expand = FALSE)
  invisible(status)
}

find_package_root <- function() {
  at_root <- function() {
    path <- getwd()
    identical(normalizePath(path), normalizePath(dirname(path)))
  }
  description_file <- "DESCRIPTION"
  while (!file.exists(description_file) && !at_root()) {
    setwd("..")
  }
  if (!file.exists(description_file)) {
    stop("Cannot find package root.")
  }
  read.dcf(description_file)
}
