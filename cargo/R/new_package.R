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
new_package <- function(path) {
  pkgname <- basename(path)
  if (!grepl("^[a-zA-Z][a-zA-Z0-9]+$", pkgname)) stop(sprintf("The name '%s' is not a valid.", pkgname))
  if (file.exists(path) || dir.exists(path)) stop(sprintf("The path '%s' already exists.", path))
  dir.create(path)
  file.copy(list.files(system.file(file.path("template"), package = "cargo"),
            all.files = TRUE, no.. = TRUE, full.names = TRUE), path, recursive = TRUE)
  file.rename(file.path(path, "DOTRbuildignore"), file.path(path, ".Rbuildignore"))
  file.rename(file.path(path, "DOTgitignore"), file.path(path, ".gitignore"))
  sed("X@X", pkgname, file.path(path, "DESCRIPTION"))
  sed("X@X", pkgname, file.path(path, "NAMESPACE"))
  install.packages(path, repos = NULL, type = "source")
}

sed <- function(pattern, replacement, filename) {
  lines <- readLines(filename)
  lines <- gsub(pattern, replacement, lines)
  writeLines(lines, filename)
}
