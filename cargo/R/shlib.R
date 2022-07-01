#' Cache a Shared Library
#'
#' This function caches a shared library developed with the Cargo framework.
#'
#' @param pkgname A string giving the name of the package.
#' @param path A string giving the path to the shared library.
#' @inheritParams run
#' @inheritParams install
#'
#' @returns A logical indicating whether the library was successfully cached.
#'
#' @seealso shlib_get
#' @export
#' @importFrom utils packageVersion
#' @examples
#' shlib_set("my_package","/some/path/to/a/shared_library.so")
#'
shlib_set <- function(pkgname, path, force=FALSE, use_packageStartupMessage=FALSE, no_prompting=FALSE) {
  cat <- function(...) {
    if ( isTRUE(use_packageStartupMessage) ) {
      packageStartupMessage(..., appendLF=FALSE)
    } else {
      base::message(..., appendLF=FALSE)
    }
  }
  if ( ! file.exists(path) ) return(FALSE)
  cache_dir <- tools::R_user_dir("cargo", "cache")
  if ( ! dir.exists(cache_dir) ) {
    unlink(cache_dir)
    message <- sprintf(
      'The cargo package would like to write to the directory:
    %s
This directory will then be used to: 1. cache shared libraries for R packages
based on Rust and 2. enable cached compilation for the cargo::rust_fn function.
You can revoke this permission at any time by deleting the directory.\n', cache_dir)
    if ( isFALSE(force) ) {
      if ( no_prompting ) return(invisible(FALSE))
      if ( ! interactive() ) {
        cat("Please try again in an interactive session or use 'cargo::shlib_set(..., force=TRUE)'.\n")
        return(invisible(FALSE))
      }
      while ( TRUE ) {
        cat(message)
        response <- toupper(trimws(readline(prompt="Do you agree? [y/N] ")))
        if ( response %in% c("N","") ) return(invisible(FALSE))
        if ( response %in% c("Y") ) break
        cat("\n")
      }
    } else {
      cat(message)
      cat("Agreement accepted due to 'force=TRUE'.\n")
    }
    cat("\n")
  }
  package_cache_dir <- file.path(cache_dir, "packages", pkgname)
  unlink(package_cache_dir, recursive=TRUE, expand=FALSE)
  dir.create(package_cache_dir, showWarnings=FALSE, recursive=TRUE)
  package_details_file <- file.path(package_cache_dir, "details.txt")
  r_major_minor <- paste0(R.version$major,".",gsub("[.].*","",R.version$minor))
  pkg_version <- as.character(utils::packageVersion(pkgname))
  writeLines(c("1",r_major_minor,pkg_version), package_details_file)
  library_name <- paste0(pkgname, .Platform$dynlib.ext)
  library_path <- file.path(package_cache_dir, library_name)
  file.copy(path, library_path, overwrite=TRUE)
}

#' Cache a Shared Library
#'
#' This function retrieves the path to a cached shared library developed with
#' the Cargo framework.
#'
#' @param pkgname A string giving the name of the package.
#'
#' @returns A string giving the path to the cached shared library, or
#'   \code{NULL} if the library is not cached.
#'
#' @seealso shlib_set
#' @export
#' @examples
#' shlib_get("my_package")
#'
shlib_get <- function(pkgname) {
  cache_dir <- tools::R_user_dir("cargo", "cache")
  package_cache_dir <- file.path(cache_dir, "packages", pkgname)
  package_details_file <- file.path(package_cache_dir, "details.txt")
  if ( ! file.exists(package_details_file) ) return(NULL)
  details <- readLines(package_details_file)
  if ( identical(details[1], "1") ) { # Details specification version 1
    names(details) <- c("specification", "r_version", "pkg_version")
    r_major_minor <- paste0(R.version$major,".",gsub("[.].*","",R.version$minor))
    pkg_version <- as.character(utils::packageVersion(pkgname))
    if ( ( details['r_version'] == r_major_minor ) && ( details['pkg_version'] == pkg_version ) ) {
      library_name <- paste0(pkgname, .Platform$dynlib.ext)
      library_path <- file.path(package_cache_dir, library_name)
      if ( ! file.exists(library_path) ) return(NULL)
      library_path
    } else {
      NULL
    }
  } else {
    NULL
  }
}
