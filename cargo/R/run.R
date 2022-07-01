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
#'   found from the field: \code{SystemRequirements: Cargo (>= XXXX)}.
#' @param methods A character vector potentially containing values
#'   \code{"envir"}, \code{"path"}, and \code{"cache"}. This indicates the
#'   methods to use (and their order) when searching for a suitable Cargo
#'   installation. \code{"envir"} indicates to try to use the values of the
#'   \code{CARGO_HOME} and \code{RUSTUP_HOME} environment variables.
#'   \code{"path"} indicates to try to use the directories \code{.cargo} and
#'   \code{.rustup} in the user's home directory. \code{"cache"} indicates to
#'   try to use the directories from the cargo package's own installation as
#'   given by the \code{tools::R_user_dir('cargo', 'cache')}.
#' @param environment_variables A named character vector providing environment
#'   variables which should be temporarily set while running Cargo.  Note that
#'   \code{RUSTUP_HOME} and \code{CARGO_HOME} are automatically set by this
#'   function.
#' @param rustflags A character vector from which the
#'   \code{CARGO_ENCODED_RUSTFLAGS} environment variables is constructed and
#'   then temporarily set. Or, if \code{NULL}, this environment variable is left
#'   unchanged.
#' @param use_packageStartupMessage Should essential messages be displayed using
#'   [base::packageStartupMessage()]?
#' @param must_be_silent Should all messages be suppressed (regardless of the
#'   value of \code{use_packageStartupMessage})?
#' @param no_prompting Prohibit prompting the user?
#' @param stdout See argument of the same name in [base::system2()].
#' @param stderr See argument of the same name in [base::system2()].
#'
#' @return The same value and behavior as the [base::system2()] function, except
#'   a non-zero exit code will be given in Cargo is not found.
#'
#' @export
#'
#' @examples
#' if ( run("--version", must_be_silent=TRUE) != 0 ) {
#'     message("Cargo is not installed. Please run cargo::install() in an interactive session.")
#' }
#'
run <- function(..., minimum_version=".", methods=c("envir","path","cache"), environment_variables=list(), rustflags=NULL, use_packageStartupMessage=FALSE, must_be_silent=FALSE, no_prompting=FALSE, stdout="", stderr="") {
  args <- shQuote(c(...))
  msg <- function(...) {
    if ( must_be_silent ) return()
    if ( use_packageStartupMessage ) {
      packageStartupMessage(..., appendLF=FALSE)
    } else {
      base::message(..., appendLF=FALSE)
    }
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
  check_candidate <- function(cargo_home, rustup_home, can_install=FALSE, can_update=FALSE) {
    vars <- c(get_homes(cargo_home, rustup_home), mk_rustflags(rustflags), environment_variables)
    cargo_cmd <- file.path(cargo_home, "bin", paste0("cargo", ifelse(windows,".exe","")))
    if ( ! file.exists(cargo_cmd) ) {
      if ( ! can_install ) return(201)
      if ( ! install_engine(FALSE, use_packageStartupMessage, no_prompting) ) {
        return(202)
      }
    }
    output <- system3(cargo_cmd, "--version", stdout=TRUE, env=vars)
    if ( ! is.null(attr(output,"status")) ) {
      msg("Cargo is installed, but broken.\nPlease try again by running 'cargo::install()' in an interactive session.\n")
      return(203)
    }
    version <- tryCatch({
      version <- strsplit(output," ",fixed=TRUE)[[1]][2]
      if ( is.na(version) ) {
        msg(sprintf("Problem parsing Cargo version string: '%s'.\nPlease try again by running 'cargo::install()' in an interactive session.\n",paste(output,collapse=",")))
        return(204)
      }
      if ( utils::compareVersion(version, msrv) < 0 ) {
        msg(sprintf("Cargo version '%s' is installed, but '%s' is needed. Trying to update...\n",version,msrv))
        if ( ! can_update ) {
          msg("Upgrading this version is not permitted.\n")
          return(205)
        }
        rustup_cmd <- file.path(cargo_home, "bin", paste0("rustup", ifelse(windows,".exe","")))
        exit_status <- system3(rustup_cmd, "update", env=vars)
        if ( exit_status != 0 ) {
          msg("Upgrade failed.\nPlease try again by running 'cargo::install()' in an interactive session.\n")
          return(exit_status)
        }
      } else {
        msg(sprintf("Cargo version '%s' is installed, which satisfies the need for '%s'.\n",version,msrv))
      }
      version
    }, warning=identity, error=identity)
    if ( inherits(version,"warning") || inherits(version,"error") ) {
      msg(sprintf("Problem parsing Cargo version string '%s', comparing it against '%s', or running 'rustup update'.\nPlease try again by running 'cargo::install()' in an interactive session.\n",paste(output,collapse=","),msrv))
      return(206)
    }
    0
  }
  windows <- .Platform$OS.type=="windows"
  run_engine <- function(bypass_env_var, condition, cargo_home, rustup_home, can_install, can_update) {
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
      status <- check_candidate(cargo_home, rustup_home, can_install, can_update)
      if ( status == 0 ) {
        cargo_cmd <- file.path(cargo_home, "bin", paste0("cargo", ifelse(windows,".exe","")))
        msg(sprintf("Cargo found at: %s\n", cargo_cmd))
        vars <- c(get_homes(cargo_home, rustup_home), mk_rustflags(rustflags), environment_variables)
        return(system3(cargo_cmd, args, env=vars, stdout=stdout, stderr=stderr))
      } else {
        msg("Method failed.\n")
      }
    } else {
      msg("Condition not met.\n")
    }
    NULL
  }
  for ( method in methods ) {
    if ( method == "envir" ) {
      msg("Trying to find a suitable Cargo using the CARGO_HOME and RUSTUP_HOME environment variables.\n")
      status <- run_engine(
        "R_CARGO_RUN_ENVIR",
        ( Sys.getenv("CARGO_HOME","<unset>") != "<unset>" ) && ( Sys.getenv("RUSTUP_HOME","<unset>") != "<unset>" ),
        Sys.getenv("CARGO_HOME"), Sys.getenv("RUSTUP_HOME"), FALSE, FALSE)
      if ( ( ! is.null(status) ) && ( status == 0 ) ) return(status)
    } else if ( method == "path" ) {
      prefix <- ifelse(windows,"%USERPROFILE%","$HOME")
      msg(sprintf("Trying to find a suitable Cargo at %s/.cargo and %s/.rustup.\n", prefix, prefix))
      prefix_dir <- Sys.getenv(ifelse(windows,"USERPROFILE","HOME"))
      status <- run_engine(
        "R_CARGO_RUN_PATH",
        Sys.getenv(ifelse(windows,"USERPROFILE","HOME"),"<unset>") != "<unset>",
        file.path(prefix_dir, ".cargo"), file.path(prefix_dir, ".rustup"), FALSE, FALSE)
      if ( ( ! is.null(status) ) && ( status == 0 ) ) return(status)
    } else if ( method == "cache" ) {
      prefix_dir <- tools::R_user_dir("cargo", "cache")
      msg("Trying to find a suitable Cargo using tools::R_user_dir('cargo', 'cache').\n")
      status <- run_engine(
        "R_CARGO_RUN_CACHE",
        TRUE,
        file.path(prefix_dir, "cargo"), file.path(prefix_dir, "rustup"), TRUE, TRUE)
      if ( ( ! is.null(status) ) && ( status == 0 ) ) return(status)
    }
  }
  msg("Cargo not found.\n")
  100
}
