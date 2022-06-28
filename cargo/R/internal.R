# An enhancement of the system2 function which sets environment variables better.
system3 <- function(..., env=character()) {
  if ( length(env) > 0 ) {
    names <- names(env)
    original_env <- sapply(names, function(x) Sys.getenv(x,"<unset>"))
    tmp <- original_env != "<unset>"
    to_restore <- original_env[tmp]
    to_unset <- names(original_env[!tmp])
    tmp <- ! is.na(env)
    if ( sum(tmp) > 0 ) do.call(Sys.setenv, as.list(env[tmp]))
    if ( sum(!tmp) > 0 ) Sys.unsetenv(names(env[!tmp]))
    on.exit({
      if ( length(to_restore) > 0 ) do.call(Sys.setenv, as.list(to_restore))
      Sys.unsetenv(to_unset)
    })
  }
  system2(...)
}

get_homes <- function(cargo_home, rustup_home) {
  c(CARGO_HOME=normalizePath(cargo_home, mustWork=FALSE),
    RUSTUP_HOME=normalizePath(rustup_home, mustWork=FALSE))
}

mk_rustflags <- function(...) {
  args <- c(...)
  if ( is.null(args) ) list()
  else {
    x <- if ( any(is.na(args)) || (length(args) == 0) ) {
      NA
    } else {
      paste(args, collapse=rawToChar(as.raw(strtoi("1f",base=16L))))
    }
    list(CARGO_ENCODED_RUSTFLAGS=x)
  }
}
