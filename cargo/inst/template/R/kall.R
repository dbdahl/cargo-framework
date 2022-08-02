.Kall <- function(...) {
  x <- .Call(...)
  if ( inherits(x,"error") ) stop(x) else x
}
