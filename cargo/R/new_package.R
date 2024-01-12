#' Make a Skeleton for a New Package
#'
#' A new Rust-based package is created at the supplied path and the package is installed.
#'
#' @param path A path where the package is created.  The name of the
#'   package is taken as the last element in the file path.
#' @param revision A git revision to use.  Defaults to "main".
#' @param include_justfile Should the new package have a `justfile` for convenience?'
#'
#' @export
#' @importFrom utils install.packages
#'
new_package <- function(path, revision = "main", include_justfile = FALSE) {
  pkgname <- basename(path)
  if (!grepl("^[a-zA-Z][a-zA-Z0-9]+$", pkgname)) stop(sprintf("The name '%s' is not a valid.", pkgname))
  if (file.exists(path) || dir.exists(path)) stop(sprintf("The path '%s' already exists.", path))
  x <- download_roxido_example(revision = revision)
  on.exit(add = TRUE, {
    unlink(dirname(x), recursive = TRUE, force = TRUE, expand = FALSE)
  })
  y <- file.path(dirname(x), pkgname)
  if (!file.rename(x, y)) {
    stop(sprintf("Problem renaming directory '%s' to '%s'.", x, y))
  }
  z <- dirname(path)
  if (!file.copy(y, z, recursive = TRUE)) {
    stop(sprintf("Problem copying directory '%s' to '%s'.", y, z))
  }
  sed("roxidoExample", pkgname, file.path(path, "DESCRIPTION"))
  sed("roxidoExample", pkgname, file.path(path, "NAMESPACE"))
  if (!isTRUE(include_justfile)) {
    file.remove(file.path(path, "justfile"))
  }
  install.packages(path, repos = NULL, type = "source")
}

sed <- function(pattern, replacement, filename) {
  lines <- readLines(filename)
  lines <- gsub(pattern, replacement, lines)
  writeLines(lines, filename)
}
