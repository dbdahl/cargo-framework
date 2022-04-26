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
#' # R_CARGO \dontrun{
#' # R_CARGO # Example disabled since Cargo was not found when installing from source package.
#' # R_CARGO # You can still run the example if you install Cargo. Hint: cargo::install().
#' myrnorm(10, 5, 4)
#' # R_CARGO }
myrnorm <- function(n, mean, sd) {
  .Call(.myrnorm, n, mean, sd)
}
