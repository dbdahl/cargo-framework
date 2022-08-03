#' Run Cargo
#'
#' This function runs Cargo (Rust's package manager) with the \code{...}
#' arguments passed as command line arguments.
#'
#' @param ... Character vector of command line arguments passed to the
#'   \code{cargo} command.
#' @param minimum_version A character string representing the minimum version of
#'   Rust that is needed. Or a path to the root of a package (i.e., the
#'   directory containing the DESCRIPTION file), in which case the value is
#'   found from the field: \code{SystemRequirements: Cargo (>= XXXX)}. For the
#'   \code{search_methods} being \code{"cache"}, the shell command \code{rustup}
#'   is used to upgrade the Cargo installation if needed.
#' @param search_methods A character vector potentially containing values
#'   \code{"path"}, \code{"convention"}, and \code{"cache"}. This indicates the
#'   methods to use (and their order) when searching for a suitable Cargo
#'   installation. \code{"path"} indicates to try to use [base::Sys.which()].
#'   \code{"convention"} indicates to try to use the directories \code{.cargo}
#'   in the user's home directory. \code{"cache"} indicates to try to use the
#'   directory from the cargo package's own installation as given by the
#'   \code{tools::R_user_dir('cargo', 'cache')}.
#' @param cargo_home_method A string equal to \code{"default"}, \code{"cache"},
#'   or \code{"tempdir"}. If \code{"default"}, the \code{CARGO_HOME} environment
#'   variable is not set. If \code{"cache"}, the \code{CARGO_HOME} environment
#'   variable is set to the subdirectory from the cargo package's own
#'   installation as given by the \code{tools::R_user_dir('cargo', 'cache')}. If
#'   \code{"tempdir"}, the \code{CARGO_HOME} environment variable is set to a
#'   temporary directory that is subsequently deleted.
#' @param environment_variables A named character vector providing environment
#'   variables which should be temporarily set while running Cargo.  Note that
#'   the \code{CARGO_HOME} environment variable is automatically set when
#'   \code{cargo_home_method != "default"} so if you want to explicitly set it
#'   here, use \code{cargo_home_method = "default"}.
#' @param rustflags A character vector from which the
#'   \code{CARGO_ENCODED_RUSTFLAGS} environment variables is constructed and
#'   then temporarily set. Or, if \code{NULL}, this environment variable is left
#'   unchanged.
#' @param stdout See argument of the same name in [base::system2()].
#' @param stderr See argument of the same name in [base::system2()].
#'
#' @return The same value and behavior as the [base::system2()] function, except
#'   a non-zero exit code will be given in Cargo is not found.
#'
#' @export
#'
#' @examples
#' if ( run("--version") != 0 ) {
#'     message("Cargo is not installed. Please run cargo::install() in an interactive session.")
#' }
#'
run <- function(..., minimum_version=".", search_methods=c("path","convention","cache"), cargo_home_method=c("default","cache","tempdir")[1], environment_variables=list(), rustflags=NULL, verbose=TRUE, stdout="", stderr="") {
  args <- shQuote(c(...))
  msg <- function(...) {
    if ( ! isFALSE(verbose) ) base::message(..., appendLF=FALSE)
    if ( inherits(verbose,"connection") ) writeLines(c(...), con=verbose, sep="")
  }
  desc_file <- file.path(minimum_version, "DESCRIPTION")
  msrv <- if ( file.exists(desc_file) ) {
    desc <- read.dcf(desc_file)
    x <- tryCatch(as.character(desc[,"SystemRequirements"]), error=function(e) NA)
    if ( is.na(x) ) {
      msg("Could not find 'SystemRequirements' field in DESCRIPTION file.\n")
      return(100)
    }
    y <- gsub(".*[Cc]argo\\s*\\(>=\\s*([^)]+)\\).*","\\1", x)
    if ( identical(x,y) ) {
      msg("Could not find expected 'SystemRequirements: Cargo (>= XXXX)' in DESCRIPTION file.")
      return(101)
    }
    gsub("\\s*([^ ]+)\\s*","\\1",y)
  } else if ( minimum_version == "." ) "1.31.0" else minimum_version
  check_candidate <- function(cargo_cmd, vars, can_update=FALSE) {
    msg(sprintf("Trying to use Cargo at: %s\n", cargo_cmd))
    if ( ! file.exists(cargo_cmd) ) {
      msg("Not found.\n")
      return(201)
    }
    output <- system3(cargo_cmd, "--version", stdout=TRUE, env=vars)
    if ( ! is.null(attr(output,"status")) ) {
      msg("Cargo is installed, but broken.\nPlease try again after running 'cargo::install()' in an interactive session.\n")
      return(202)
    }
    version <- tryCatch({
      version <- strsplit(output," ",fixed=TRUE)[[1]][2]
      if ( is.na(version) ) {
        msg(sprintf("Problem parsing Cargo version string: '%s'.\nPlease try again after running 'cargo::install()' in an interactive session.\n",paste(output,collapse=",")))
        return(203)
      }
      if ( utils::compareVersion(version, msrv) < 0 ) {
        msg(sprintf("Cargo version '%s' is available, but '%s' is needed.\n",version,msrv))
        if ( ! can_update ) {
          rustup_path <- Sys.which("rustup")
          if ( rustup_path != "" ) {
            msg(sprintf("You could run '%s' to update the Cargo installation.\n", rustup_path))
            return(204)
          } else {
            msg("Cannot upgrade this Cargo installation.\n")
            return(205)
          }
        } else {
          msg("Trying to upgrade this Cargo installation.\n")
        }
        rustup_cmd <- file.path(dirname(cargo_cmd), paste0("rustup", ifelse(windows,".exe","")))
        cargo_home <- dirname(dirname(cargo_cmd))
        exit_status <- system3(rustup_cmd, "update", env=c(vars, CARGO_HOME=cargo_home, RUSTUP_HOME=file.path(dirname(cargo_home),"rustup")))
        if ( exit_status != 0 ) {
          msg("Upgrade failed.\nPlease try again by running 'cargo::install()' in an interactive session.\n")
          return(exit_status)
        }
        return(Recall(cargo_cmd, vars, FALSE))
      } else {
        msg(sprintf("Cargo version '%s' is available, which satisfies the need for '%s'.\n",version,msrv))
      }
      version
    }, warning=identity, error=identity)
    if ( inherits(version,"warning") || inherits(version,"error") ) {
      msg(sprintf("Problem parsing Cargo version string '%s', comparing it against '%s', or running 'rustup update'.\nPlease try again after running 'cargo::install()' in an interactive session.\n",paste(output,collapse=","),msrv))
      return(206)
    }
    0
  }
  windows <- .Platform$OS.type=="windows"
  cargo_home_env <- if ( cargo_home_method == "cache" ) {
    prefix_dir <- Sys.getenv(ifelse(windows,"USERPROFILE","HOME"))
    file.path(prefix_dir, ".cargo")
  } else if ( cargo_home_method == "tempdir" ) {
    temp_dir <- tempdir(check=TRUE)
    target_dir <- tempfile(pattern="rust-", tmpdir=temp_dir)
    on.exit({
      unlink(target_dir, recursive=TRUE, force=TRUE, expand=FALSE)
    })
    target_dir
  }
  run_engine <- function(bypass_env_var, condition, cargo_cmd, can_update) {
    general_bypass_env_var <- "R_CARGO_RUN"
    if ( toupper(Sys.getenv(general_bypass_env_var, "TRUE")) == "FALSE" ) {
      msg(sprintf("Method bypassed by %s environment variable.\n", general_bypass_env_var))
      return(NULL)
    }
    if ( toupper(Sys.getenv(bypass_env_var, "TRUE")) == "FALSE" ) {
      msg(sprintf("Method bypassed by %s environment variable.\n", bypass_env_var))
      return(NULL)
    }
    if ( condition ) {
      cargo_cmd <- normalizePath(cargo_cmd, mustWork=FALSE)
      vars <- c(CARGO_HOME=cargo_home_env, mk_rustflags(rustflags), environment_variables)
      status <- check_candidate(cargo_cmd, vars, can_update)
      if ( status == 0 ) {
        result <- system3(cargo_cmd, args, env=vars, stdout=stdout, stderr=stderr)
        msg("---\n")
        return(result)
      } else {
        msg("Method failed.\n")
      }
    } else {
      msg("Condition not met for method.\n")
    }
    msg("---\n")
    NULL
  }
  msg("---\n")
  for ( method in search_methods ) {
    if ( method == "path" ) {
      msg("Trying to find a suitable Cargo using the PATH environment variable.\n")
      status <- run_engine("R_CARGO_RUN_PATH", TRUE, Sys.which("cargo"), FALSE)
      if ( ( ! is.null(status) ) && ( ! is.numeric(status) || ( status == 0 ) ) ) return(status)
    } else if ( method == "convention" ) {
      msg("Trying to find a suitable Cargo using the conventional location.\n")
      prefix_dir <- Sys.getenv(ifelse(windows,"USERPROFILE","HOME"))
      status <- run_engine(
        "R_CARGO_RUN_CONVENTION",
        Sys.getenv(ifelse(windows,"USERPROFILE","HOME"),"<unset>") != "<unset>",
        file.path(prefix_dir, ".cargo", "bin", paste0("cargo", ifelse(windows,".exe",""))), FALSE)
      if ( ( ! is.null(status) ) && ( ! is.numeric(status) || ( status == 0 ) ) ) return(status)
    } else if ( method == "cache" ) {
      prefix_dir <- tools::R_user_dir("cargo", "cache")
      msg("Trying to find a suitable Cargo using tools::R_user_dir('cargo', 'cache').\n")
      status <- run_engine(
        "R_CARGO_RUN_CACHE",
        TRUE,
        file.path(prefix_dir, "cargo", "bin", paste0("cargo", ifelse(windows,".exe",""))), TRUE)
      if ( ( ! is.null(status) ) && ( ! is.numeric(status) || ( status == 0 ) ) ) return(status)
    }
  }
  msg("No suitable version of Cargo was found.\nOne solution is to run 'install()' from the 'cargo' package.\n---\n")
  1
}

# An enhancement of the system2 function which sets environment variables better.
system3 <- function(..., env=character()) {
  if ( length(env) > 0 ) {
    names <- names(env)
    original_env <- sapply(names, function(x) Sys.getenv(x,"<unset>"))
    tmp <- original_env != "<unset>"
    to_restore <- original_env[tmp]
    to_unset <- names(original_env[!tmp])
    tmp <- ! is.na(env)
    if ( sum(tmp) > 0 ) do.call(Sys.setenv, as.list(env[tmp]))
    if ( sum(!tmp) > 0 ) Sys.unsetenv(names(env[!tmp]))
    on.exit({
      if ( length(to_restore) > 0 ) do.call(Sys.setenv, as.list(to_restore))
      Sys.unsetenv(to_unset)
    })
  }
  system2(...)
}

mk_rustflags <- function(...) {
  args <- c(...)
  if ( is.null(args) ) list()
  else {
    x <- if ( any(is.na(args)) || (length(args) == 0) ) {
      NA
    } else {
      paste(args, collapse=rawToChar(as.raw(strtoi("1f",base=16L))))
    }
    list(CARGO_ENCODED_RUSTFLAGS=x)
  }
}
