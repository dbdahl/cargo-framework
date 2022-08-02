#' Convolve Two Vectors
#'
#' This function demonstrates a Rust implementation of the convolution algorithm
#' shown in Section 5.10.1 "Calling .Call" of [*Writing R
#' Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html#Calling-_002eCall).
#'
#' @param a A numeric vector.
#' @param b A numeric vector.
#'
#' @return A numeric vector.
#' @export
#' @examples
#' convolve2(c(1,2,3), c(2,4,8))
convolve2 <- function(a, b) {
  .Call(.convolve2, a, b)
}
