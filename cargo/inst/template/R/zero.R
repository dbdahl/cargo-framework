#' Zero-Finding
#'
#' This function demonstrates a Rust implementation of the zero-finding
#' algorithm shown in Section 5.11.1 "Zero-finding" of [*Writing R
#' Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html#Zero_002dfinding)
#'
#' @param f A function taking a numeric vector of length one.
#' @param guesses A numerical vector of length two giving two guesses for the
#'   zero of the function.  The sign of the function evaluated at these two
#'   guesses must be opposite.
#' @param tol Tolerance controlling the desired precision.
#'
#' @export
#' @examples
#' cube1 <- function(x) (x^2 + 1) * (x - 1.5)
#' # R_CARGO \dontrun{
#' # R_CARGO # Example disabled since Cargo was not found when installing from source package.
#' # R_CARGO # You can still run the example if you install Cargo. Hint: cargo::install().
#' zero(cube1, c(-2, 11.5))
#' # R_CARGO }
zero <- function(f, guesses, tol = 1e-7) {
  f.check <- function(x) {
    x <- f(x)
    if(!is.numeric(x)) stop("Need a numeric result")
    as.double(x)
  }
  .Call(.zero,  body(f.check), as.double(guesses), as.double(tol), new.env())
}
