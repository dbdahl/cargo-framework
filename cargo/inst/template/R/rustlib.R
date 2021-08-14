#' @docType package
#' @usage NULL
#' @useDynLib X@X, .registration = TRUE
NULL

.Kall <- function(...) {
  x <- .Call(...)
  if ( inherits(x,"error") ) stop(x) else x
}
