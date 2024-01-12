#' @importFrom utils download.file untar
download_roxido_example <- function(revision) {
  owner <- "dbdahl"
  repo <- "roxidoExample"
  tarball_filename <- tempfile(sprintf("%s_%s_%s_", owner, repo, revision), fileext = ".tar.gz")
  on.exit(add = TRUE, {
    unlink(tarball_filename, recursive = TRUE, force = TRUE, expand = FALSE)
  })
  if (0 != download.file(sprintf("https://api.github.com/repos/%s/%s/tarball/%s", owner, repo, revision), tarball_filename, mode = "wb")) {
    stop("Problem downloading repository from Github.")
  }
  expand_dirname <- tempfile()
  untar(tarball_filename, exdir = expand_dirname)
  original_dirname <- list.files(expand_dirname)
  file.path(expand_dirname, original_dirname)
}
