#' Cross Compile Static Library for CRAN
#'
#' This function cross compiles the Rust static library for CRAN's target
#' platforms. The package developer can then uploaded these to a web server.
#' Then, if a particular CRAN build machine does not have a sufficient
#' installation of the Rust toolchain, the package's \file{tools/staticlib.R}
#' script can download the appropriate static library.
#'
#' @inheritParams run
#' @inheritParams register_calls
#' @param destination_directory An existing directory where the static libraries
#'   should be added.
#' @param targets A character vector giving Rust targets (e.g.
#'   \code{"x86_64-pc-windows-gnu"}). The value \code{"CRAN"} is replaced by all
#'   CRAN targets.
#'
#' @return \code{NULL}, invisibly.
#'
#' @export
#'
cross_compile <- function(destination_directory, pkgroot=".", targets="CRAN", minimum_version=file.path(pkgroot,"DESCRIPTION"), verbose=TRUE) {
  cran_targets <- c("i686-pc-windows-gnu","x86_64-pc-windows-gnu","aarch64-apple-darwin","x86_64-apple-darwin","x86_64-unknown-linux-gnu")
  if ( ! dir.exists(destination_directory) ) stop(paste0("'", destination_directory, "' does not seem to exist."))
  name_and_version <- get_name_and_version(paste0(pkgroot,"/DESCRIPTION"))
  manifest_path <- paste0(pkgroot, "/src/rustlib/Cargo.toml")
  if ( ! file.exists(manifest_path) ) stop(paste0("'", pkgroot, "' does not seem to be the package root."))
  w <- which(targets=="CRAN")
  targets <- unique(targets[!w])
  if ( any(w) ) targets <- c(targets,cran_targets)
  cwd <- getwd()
  on.exit(setwd(cwd))
  for ( target in targets ) {
    run("build", "--target", target, "--release", "--manifest-path", manifest_path, minimum_version=minimum_version, verbose=verbose)
    file.rename(paste0(pkgroot,"/src/rustlib/target/",target,"/release/librustlib.a"), paste0(pkgroot,"/src/rustlib/target/release/librustlib.a"))
    tgz_destination_dir <- paste0(destination_directory,"/",name_and_version)
    dir.create(tgz_destination_dir, showWarnings=FALSE)
    tgz_destination <- normalizePath(paste0(tgz_destination_dir,"/",target,".tar.gz"), mustWork=TRUE)
    setwd(paste0(pkgroot,"/src"))
    tar(tgz_destination, files="rustlib/target/release/librustlib.a", compression="gzip")
    setwd(cwd)
  }
  invisible(NULL)
}

get_name_and_version <- function(description_file) {
  desc <- read.dcf(description_file)
  name <- tryCatch(as.character(desc[,"Package"]), error=function(e) NA)
  if ( is.na(name) ) stop("Could not find 'Package' field in DESCRIPTION file.")
  version <- tryCatch(as.character(desc[,"Version"]), error=function(e) NA)
  if ( is.na(version) ) stop("Could not find 'Version' field in DESCRIPTION file.")
  paste0(name,"_",version)
}
