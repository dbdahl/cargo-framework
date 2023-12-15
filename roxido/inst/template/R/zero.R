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
#' zero(cube1, c(-2, 11.5))
zero <- function(f, guesses, tol = 1e-7) {
  .Call(.zero, f, guesses, tol)
}
