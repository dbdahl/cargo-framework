.onLoad <- function(libname, pkgname) {
  cache_dir <- cache_dir()
  if (!dir.exists(cache_dir)) {
    return()
  }
  # Periodically delete cached material
  last_purge_filename <- last_purge_filename()
  if (!file.exists(last_purge_filename)) {
    return()
  }
  details <- readLines(last_purge_filename)
  if (identical(details[1], "1")) { # Details specification version 1
    names(details) <- c("specification", "date", "days_until_next")
    days <- as.integer(details["days_until_next"])
    if (days > 0) {
      next_purge_threshold <- as.Date(details["date"]) + days
      if (Sys.Date() >= next_purge_threshold) purge_cache(FALSE)
    }
    if (interactive()) {
      unlink(list.files(file.path(cache_dir, "rust_fn", "R"), full.names = TRUE), recursive = FALSE, force = TRUE)
    }
  } else {
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }
}

purge_cache <- function(last_purge_filename_only) {
  cache_dir <- cache_dir()
  if (!last_purge_filename_only) {
    unlink(file.path(cache_dir, "cargo", "git"), recursive = TRUE, expand = FALSE)
    unlink(file.path(cache_dir, "cargo", "registry"), recursive = TRUE, expand = FALSE)
    unlink(file.path(cache_dir, "rust_fn"), recursive = TRUE, expand = FALSE)
  }
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(c("1", as.character(Sys.Date()), days_until_next_purge), last_purge_filename())
}

days_until_next_purge <- 91
last_purge_filename <- function() file.path(cache_dir(), "last-purge")
