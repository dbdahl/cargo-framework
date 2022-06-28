.onLoad <- function(libname, pkgname) {
  cache_dir <- tools::R_user_dir("cargo", "cache")
  if ( ! dir.exists(cache_dir) ) return()
  package_cache_dir <- file.path(cache_dir, "packages")
  if ( ! dir.exists(package_cache_dir) ) return()
  # Delete cached shared libraries for packages that are no longer installed.
  for ( package in list.files(package_cache_dir) ) {
    if ( identical(system.file(package=package),"") ) {
      unlink(file.path(package_cache_dir,package),recursive=TRUE,expand=FALSE)
    }
  }
  # Periodically delete other cached material
  last_purge_file <- file.path(cache_dir,"last-purge")
  if ( ! file.exists(last_purge_file) ) return()
  details <- readLines(last_purge_file)
  if ( identical(details[1], "1") ) { # Details specification version 1
    names(details) <- c("specification", "date", "days_until_next")
    days <- as.integer(details['days_until_next'])
    if ( days > 0 ) {
      next_purge_threshold <- as.Date(details['date']) + days
      if ( Sys.Date() >= next_purge_threshold ) purge_cache(cache_dir, days)
    }
  }
}

purge_cache <- function(cache_dir, days) {
  unlink(file.path(cache_dir,"cargo","git"), recursive=TRUE, expand=FALSE)
  unlink(file.path(cache_dir,"cargo","registry"), recursive=TRUE, expand=FALSE)
  unlink(file.path(cache_dir,"rust_fn"), recursive=TRUE, expand=FALSE)
  writeLines(c("1",as.character(Sys.Date()),days), file.path(cache_dir,"last-purge"))
}
