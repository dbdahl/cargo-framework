#' Prepare for Building the Package Source
#'
#' This function provides many tools to be used before building an R package
#' based on the Cargo Framework.
#'
#' If you update the \code{roxygen2} documentation, rerun this function
#' with \code{what="document"} to update the \code{*.Rd} files. If a
#' package's Rust code changes a dependency, rerun this function with
#' \code{what=c("authors","vendor")} to update the files
#' \code{src/rust/vendor.tar.xz} and to generate the file
#' \code{authors-scratch.txt} (which will need to be manually
#' incorporated into the \code{DESCRIPTION} file and then deleted).
#' To perform all of these actions, use \code{what="all"}.
#'
#' @param what A character vector indicating the desired action.
#' @param pkgroot The root directory of the package.
#'
#' @return \code{NULL}, invisibly.
#' @importFrom utils tar as.person
#' @export
#'
prebuild <- function(what=c("document", "vendor", "authors", "all")[4], pkgroot=".") {
  if ( "all" %in% what ) what <- c("document", "vendor", "authors")
  if ( !all(what %in% c("document", "vendor", "authors"))) {
    stop("Unrecognized value for 'what' argument.")
  }
  pkgroot <- normalizePath(pkgroot)
  description_file <- file.path(pkgroot, "DESCRIPTION")
  r_dir <- file.path(pkgroot, "R")
  src_rust_dir <- file.path(pkgroot, "src", "rust")
  if ( ! file.exists(description_file) || ! dir.exists(r_dir) || ! dir.exists(src_rust_dir) ) {
    stop(sprintf("Oops, '%s' does not appear to be a package root directory.", pkgroot))
  }
  desc <- read.dcf(description_file)
  # vendor
  vendor_engine <- function() {
    original_dir <- getwd()
    on.exit({
      setwd(original_dir)
    })
    setwd(src_rust_dir)
    config_file <- file.path(".cargo", "config.toml")
    if ( file.exists(config_file) ) unlink(config_file, expand=FALSE)
    cargo::run("update")
    dir.create(".cargo", showWarnings=FALSE)
    cargo::run("vendor", stdout=config_file)
    utils::tar("vendor.tar.xz", c("vendor",".cargo"), compression="xz", tar="internal")
  }
  if ( "vendor" %in% what ) vendor_engine()
  # authors
  authors_engine <- function() {
    original_dir <- getwd()
    on.exit({
      setwd(original_dir)
    })
    setwd(pkgroot)
    # Authors
    # Requires "cargo install cargo-authors"
    authors_crates <- function() {
      original_dir <- getwd()
      on.exit({
        setwd(original_dir)
      })
      setwd(src_rust_dir)
      x <- cargo::run("authors", stdout=TRUE)
      if ( ! is.null(attr(x,"status")) && attr(x,"status") != 0 ) {
        stop("Could not run 'cargo authors'. Install it with 'cargo::run(\"install\",\"cargo-authors\")'")
      }
      x[-(1:2)]
    }
    if ( 'Authors@R' %in% colnames(desc) ) {
      authors_crates <- authors_crates()
      authors_crates <- unlist(lapply(strsplit(authors_crates, ":"), \(x) { x <- trimws(x); y <- list(strsplit(x[2],", ")[[1]]); names(y) <- x[1]; y }), recursive=FALSE)
      crates <- cbind(names(authors_crates), sapply(authors_crates, \(x) {
        ss <- if ( length(x) > 1 ) "s" else ""
        sprintf(', role = "cph", comment = "Rust crate%s: %s.")', ss, paste(x,collapse=", "))
      }))
      authors_file <- file.path("authors-scratch.txt")
      authors_con <- file(authors_file, open="wt")
      g <- function(x) {
        if ( is.null(x) || ( length(x) == 0 ) ) '""'
        else if ( length(x) == 1 ) paste0('"',x,'"')
        else {
          paste0('"',paste(x,collapse=' '),'"')
        }
      }
      g <- function(x) paste(gsub('"','\\\\"',x),collapse=" ")
      writeLines("Authors@R: c(", authors_con)
      for ( i in seq_len(nrow(crates)) ) {
        x <- crates[i,]
        p <- as.person(x[1])
        str <- x[2]
        if ( ! is.null(p$email) ) str <- sprintf(', email = "%s"%s',p$email,str)
        gg <- g(p$given)
        gf <- g(p$family)
        str <- if ( is.null(p$given) && is.null(p$family) ) {
          stop("Both given and family names are null.")
        } else if ( is.null(p$given) ) {
          sprintf('person(family = "%s"%s',gf,str)
        } else if ( is.null(p$family) ) {
          sprintf('person(given = "%s"%s',gg,str)
        } else if ( ( tolower(p$given[1]) == "the") || ( tolower(p$family[1]) %in% c("contributors","developers") ) ) {
          sprintf('person(given = "%s"%s',paste0(gg," ",gf),str)
        } else {
          sprintf('person(given = "%s", family = "%s"%s',gg,gf,str)
        }
        cc <- if ( i != nrow(crates) ) "," else ""
        writeLines(paste0("             ",str,cc), authors_con)
      }
      writeLines("            )", authors_con)
      if (all(Sys.which(c("find","xargs","sort","uniq"))!="")) {
        others <- system("find src/rust/vendor -type f -print0 | xargs -0 grep Copyright | sort | uniq", intern=TRUE)
        writeLines("\n\nOther things to consider...", authors_con)
        writeLines(others, authors_con)
      }
      close(authors_con)
    }
  }
  if ( "authors" %in% what ) authors_engine()
  # document
  if ( "document" %in% what ) {
    using_roxygen2 <- tryCatch({
      x <- desc[,'RoxygenNote']
      !is.na(x)
    }, error=\(x) FALSE)
    if ( using_roxygen2 && requireNamespace("roxygen2", quietly=TRUE) ) {
      roxygen2::roxygenize(pkgroot)
    }
  }
  invisible(NULL)
}

