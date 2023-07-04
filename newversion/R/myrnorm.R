#' Sample from the Normal Distribution
#'
#' This is a simple little function that demonstrates calling a 'Rust' function.
#' It generates samples from the normal distribution.
#'
#' @param n An integer for the number of random samples.
#' @param mean A scalar giving the mean.
#' @param sd A scalar giving the standard deviation.
#'
#' @return A numeric vector.
#' @export
#' @examples
#' myrnorm(10, 5, 4)
myrnorm <- function(n, mean, sd) {
  .Call(.myrnorm, n, mean, sd)
}
