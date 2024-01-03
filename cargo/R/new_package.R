#' Make a Skeleton for a New Package
#'
#' A new Rust-based package is created at the supplied path and the package is installed.
#'
#' @param path A path where the package is created.  The name of the
#'   package is taken as the last element in the file path.
#'
#' @export
#' @importFrom utils install.packages
#'
new_package <- function(path, revision = "main", include_justfile = FALSE) {
  pkgname <- basename(path)
  if (!grepl("^[a-zA-Z][a-zA-Z0-9]+$", pkgname)) stop(sprintf("The name '%s' is not a valid.", pkgname))
  if (file.exists(path) || dir.exists(path)) stop(sprintf("The path '%s' already exists.", path))
  owner <- "dbdahl"
  repo <- "roxidoExample"
  tarball_filename <- tempfile(sprintf("%s_%s_%s_", owner, repo, revision), fileext = ".tar.gz")
  on.exit(add = TRUE, {
    unlink(tarball_filename, recursive = TRUE, force = TRUE, expand = FALSE)
  })
  if (0 != download.file(sprintf("https://api.github.com/repos/%s/%s/tarball/%s", owner, repo, revison), tarball_filename, mode = "wb")) {
    stop("Problem downloading repository from Github.")
  }
  expand_dirname <- tempfile()
  on.exit(add = TRUE, {
    unlink(expand_dirname, recursive = TRUE, force = TRUE, expand = FALSE)
  })
  untar(tarball_filename, exdir = expand_dirname)
  original_dirname <- list.files(expand_dirname)
  x <- file.path(expand_dirname, original_dirname)
  y <- file.path(expand_dirname, pkgname)
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
