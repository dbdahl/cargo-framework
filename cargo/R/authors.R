#' Identify Authorship of Rust Crates
#'
#' Since depending crates are vendored by the \code{\link{build_for_cran}} function, the authorship and copyright must be
#' declared in the \code{DESCRIPTION} file prior to building the source package
#' for [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/).  This function helps to identify these attributions but is not
#' guaranteed to the exhaustive, so manual inspection in warranted before submitting to CRAN.
#'
#' @return \code{NULL}, invisibly.
#' @importFrom utils as.person
#' @export
#'
authors <- function() {
  original_dir <- getwd()
  on.exit(setwd(original_dir))
  desc <- find_package_root()
  src_rust_dir <- file.path("src", "rust")
  setwd(src_rust_dir)
  # Requires "cargo install cargo-authors"
  authors_crates <- function() {
    x <- run("authors", stdout = TRUE)
    if (!is.null(attr(x, "status")) && attr(x, "status") != 0) {
      stop("Could not run 'cargo authors'. Install it with 'cargo::run(\"install\",\"cargo-authors\")'")
    }
    x[-(1:2)]
  }
  if ("Authors@R" %in% colnames(desc)) {
    authors_crates <- authors_crates()
    authors_crates <- unlist(lapply(strsplit(authors_crates, ":"), \(x) {
      x <- trimws(x)
      y <- list(strsplit(x[2], ", ")[[1]])
      names(y) <- x[1]
      y
    }), recursive = FALSE)
    crates <- cbind(names(authors_crates), sapply(authors_crates, \(x) {
      ss <- if (length(x) > 1) "s" else ""
      sprintf(', role = "cph", comment = "Rust crate%s: %s.")', ss, paste(x, collapse = ", "))
    }))
    # authors_file <- file.path("authors-scratch.txt")
    authors_con <- stdout() # file(authors_file, open = "wt")
    g <- function(x) {
      if (is.null(x) || (length(x) == 0)) {
        '""'
      } else if (length(x) == 1) {
        paste0('"', x, '"')
      } else {
        paste0('"', paste(x, collapse = " "), '"')
      }
    }
    g <- function(x) paste(gsub('"', '\\\\"', x), collapse = " ")
    writeLines("Authors@R: c(", authors_con)
    for (i in seq_len(nrow(crates))) {
      x <- crates[i, ]
      p <- as.person(x[1])
      str <- x[2]
      if (!is.null(p$email)) str <- sprintf(', email = "%s"%s', p$email, str)
      gg <- g(p$given)
      gf <- g(p$family)
      str <- if (is.null(p$given) && is.null(p$family)) {
        stop("Both given and family names are null.")
      } else if (is.null(p$given)) {
        sprintf('person(family = "%s"%s', gf, str)
      } else if (is.null(p$family)) {
        sprintf('person(given = "%s"%s', gg, str)
      } else if ((tolower(p$given[1]) == "the") || (tolower(p$family[1]) %in% c("contributors", "developers"))) {
        sprintf('person(given = "%s"%s', paste0(gg, " ", gf), str)
      } else {
        sprintf('person(given = "%s", family = "%s"%s', gg, gf, str)
      }
      cc <- if (i != nrow(crates)) "," else ""
      writeLines(paste0("             ", str, cc), authors_con)
    }
    writeLines("            )", authors_con)
    pkgname <- desc[, "Package"]
    tarball_name <- paste0(pkgname, "_", desc[, "Version"], ".tar.gz")
    writeLines(paste0("\nYou can also browse the contents of src/rust/vendor.tar.xz in the tarball ", tarball_name, ".\nFor example, in your shell, run commands like this:\n"), authors_con)
    tmp <- dirname(tempdir())
    writeLines(sprintf("    tar -x -C %s -f %s %s/src/rust/vendor.tar.xz", tmp, tarball_name, pkgname), authors_con)
    writeLines(sprintf("    cd %s/%s/src/rust/", tmp, pkgname), authors_con)
    writeLines("    tar -x -f vendor.tar.xz", authors_con)
    writeLines(sprintf("    cd vendor"), authors_con)
    writeLines("    find -type f -print0 | xargs -0 grep Copyright | sort | uniq", authors_con)
    writeLines(sprintf("    cd %s", tmp), authors_con)
    writeLines(sprintf("    rm -rf %s", pkgname), authors_con)
    writeLines("", authors_con)
    if (!identical(stdout(), authors_con)) close(authors_con)
  }
  invisible(NULL)
}
