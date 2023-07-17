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
#' @param leave_no_trace If \code{TRUE}, the \code{CARGO_HOME} environment
#'   variable is set to a temporary directory that is subsequently deleted.
#' @param environment_variables A named character vector providing environment
#'   variables which should be temporarily set while running Cargo.  Note that
#'   the \code{CARGO_HOME} and \code{RUSTUP_HOME} environment variables are
#'   automatically set when using the \code{"cache"} search method.  Also, the
#'   \code{CARGO_HOME} environment variable is also set when
#'   \code{leave_no_trace == TRUE}.
#' @param rustflags A character vector from which the
#'   \code{CARGO_ENCODED_RUSTFLAGS} environment variables is constructed and
#'   then temporarily set. Or, if \code{NULL}, this environment variable is left
#'   unchanged.
#' @param verbose If \code{TRUE}, details of the search for Cargo are shown. If
#'   \code{FALSE}, no details are shown.  If it is a connection, then details
#'   are shown and also written to the connection.
#' @param run_twice Should the cargo command be run twice? The environment
#'   variable \code{R_CARGO_RUN_COUNTER} is set to either \code{1} or \code{2}
#'   during each run.
#' @param stdout See argument of the same name in [base::system2()].
#' @param stderr See argument of the same name in [base::system2()].
#'
#' @return The same value and behavior as the [base::system2()] function, except
#'   a non-zero exit code will be given in Cargo is not found.
#'
#' @export
#'
#' @examples
#' if (run("--version") != 0) {
#'   message("Cargo is not installed. Please run cargo::install() in an interactive session.")
#' }
#'
run <- function(..., minimum_version = ".", search_methods = c("cache", "convention", "path"), leave_no_trace = FALSE, environment_variables = list(),
                rustflags = NULL, verbose = TRUE, run_twice = FALSE, stdout = "", stderr = "") {
  args <- shQuote(c(...))
  msg <- function(...) {
    if (!isFALSE(verbose)) base::message(..., appendLF = FALSE)
    if (inherits(verbose, "connection")) writeLines(c(...), con = verbose, sep = "")
  }
  desc_file <- file.path(minimum_version, "DESCRIPTION")
  msrv <- if (file.exists(desc_file)) {
    desc <- read.dcf(desc_file)
    x <- tryCatch(as.character(desc[, "SystemRequirements"]), error = function(e) NA)
    if (is.na(x)) {
      msg("Could not find 'SystemRequirements' field in DESCRIPTION file.\n")
      return(100)
    }
    if (!grepl("([Cc]argo|[Rr]ustc)\\s*[(]>=", x)) {
      msg("In DESCRIPTION file, could not find 'SystemRequirements: rustc (>= XXXX)' or 'SystemRequirements: Cargo (>= XXXX)'.")
      return(101)
    }
    sub("\\D*\\).*", "", sub(".*[(,]\\s*>=\\D*", "", x))
  } else {
    if (minimum_version == ".") "1.31.0" else minimum_version
  }
  windows <- .Platform$OS.type == "windows"
  environment_variables <- c(environment_variables, mk_rustflags(rustflags))
  environment_variables <- if (leave_no_trace) {
    if ("CARGO_HOME" %in% names(environment_variables)) {
      msg("CARGO_HOME should not be provided in 'environment_variables' when using 'leave_no_trace = TRUE'.")
      return(102)
    }
    temp_dir <- tempdir(check = TRUE)
    target_dir <- tempfile(pattern = "rust-", tmpdir = temp_dir)
    on.exit({
      unlink(target_dir, recursive = TRUE, force = TRUE, expand = FALSE)
    })
    c(environment_variables, CARGO_HOME = target_dir)
  } else {
    environment_variables
  }
  run_engine <- function(bypass_env_var, cargo_cmd, vars, can_update) {
    general_bypass_env_var <- "R_CARGO_RUN"
    if (toupper(Sys.getenv(general_bypass_env_var, "TRUE")) == "FALSE") {
      msg(sprintf("Method bypassed by %s environment variable.\n", general_bypass_env_var))
      return(NULL)
    }
    if (toupper(Sys.getenv(bypass_env_var, "TRUE")) == "FALSE") {
      msg(sprintf("Method bypassed by %s environment variable.\n", bypass_env_var))
      return(NULL)
    }
    cargo_cmd <- normalizePath(cargo_cmd, mustWork = FALSE)
    check_candidate <- function() {
      msg(sprintf("Trying to use Cargo at: %s\n", cargo_cmd))
      if (!file.exists(cargo_cmd)) {
        msg("Not found.\n")
        return(201)
      }
      output <- system3(cargo_cmd, "--version", stdout = TRUE, env = vars)
      if (!is.null(attr(output, "status"))) {
        msg("Cargo is installed, but broken.\nPlease try again after running 'cargo::install()' in an interactive session.\n")
        return(202)
      }
      version <- tryCatch(
        {
          version <- strsplit(output, " ", fixed = TRUE)[[1]][2]
          if (is.na(version)) {
            msg(sprintf(
              "Problem parsing Cargo version string: '%s'.\nPlease try again after running 'cargo::install()' in an interactive session.\n",
              paste(output, collapse = ",")
            ))
            return(203)
          }
          if (utils::compareVersion(version, msrv) < 0) {
            msg(sprintf("Cargo version '%s' is available, but '%s' is needed.\nPlease upgrade your Cargo installation.\n", version, msrv))
            rustup_cmd <- normalizePath(file.path(dirname(cargo_cmd), paste0("rustup", ifelse(windows, ".exe", ""))), mustWork = FALSE)
            if (!can_update) {
              if (file.exists(rustup_cmd)) {
                msg(sprintf("Hint: You can run '%s' to update the Cargo installation.\n", rustup_cmd))
                return(204)
              }
              return(205)
            }
            msg("Trying to upgrade this Cargo installation.\n")
            vars2 <- if ("CARGO_HOME_ORIGINAL" %in% names(vars)) {
              vars2 <- vars
              vars2[["CARGO_HOME"]] <- vars2[["CARGO_HOME_ORIGINAL"]]
              vars2[names(vars2) != "CARGO_HOME_ORIGINAL"]
            } else {
              vars
            }
            exit_status <- system3(rustup_cmd, "update", env = vars2)
            if (exit_status != 0) {
              msg("Upgrade failed.\nPlease try again by running 'cargo::install()' in an interactive session.\n")
              return(exit_status)
            }
            return(Recall())
          } else {
            msg(sprintf("Cargo version '%s' is available, which satisfies the need for '%s'.\n", version, msrv))
          }
          version
        },
        warning = identity,
        error = identity
      )
      if (inherits(version, "warning") || inherits(version, "error")) {
        msg(sprintf(
          "Problem parsing Cargo version string '%s', comparing it against '%s', or other error.\nPlease try again after running 'cargo::install()' in an interactive session.\n",
          paste(output, collapse = ","), msrv
        ))
        return(206)
      }
      0
    }
    status <- check_candidate()
    if (status == 0) {
      result_first <- system3(cargo_cmd, args, env = c(vars, R_CARGO_RUN_COUNTER = 1), stdout = stdout, stderr = stderr)
      result <- if (run_twice) {
        system3(cargo_cmd, args, env = c(vars, R_CARGO_RUN_COUNTER = 2), stdout = stdout, stderr = stderr)
      } else {
        result_first
      }
      msg("---\n")
      return(result)
    } else {
      msg("Method failed.\n")
    }
    msg("---\n")
  }
  msg("---\n")
  for (method in search_methods) {
    if (method == "path") {
      msg("Trying to find a suitable Cargo using the PATH environment variable.\n")
      cargo_cmd <- Sys.which("cargo")
      if (is.na(cargo_cmd) || (cargo_cmd == "")) next
      vars <- environment_variables
      status <- run_engine("R_CARGO_RUN_PATH", cargo_cmd, vars, FALSE)
      if ((!is.null(status)) && (!is.numeric(status) || (status == 0))) {
        return(status)
      }
    } else if (method == "convention") {
      msg("Trying to find a suitable Cargo using the conventional location.\n")
      prefix_dir <- Sys.getenv(ifelse(windows, "USERPROFILE", "HOME"), "<unset>")
      if (prefix_dir %in% c("<unset>", "")) next
      cargo_bin_dir <- file.path(prefix_dir, ".cargo", "bin")
      if (!dir.exists(cargo_bin_dir)) next
      cargo_cmd <- file.path(cargo_bin_dir, paste0("cargo", ifelse(windows, ".exe", "")))
      vars <- c(environment_variables,
        PATH = paste0(Sys.getenv("PATH"), .Platform$path.sep, cargo_bin_dir)
      )
      status <- run_engine("R_CARGO_RUN_CONVENTION", cargo_cmd, vars, FALSE)
      if ((!is.null(status)) && (!is.numeric(status) || (status == 0))) {
        return(status)
      }
    } else if (method == "cache") {
      msg("Trying to find a suitable Cargo using tools::R_user_dir('cargo', 'cache').\n")
      prefix_dir <- cache_dir()
      cargo_home <- file.path(prefix_dir, "cargo")
      cargo_bin_dir <- file.path(cargo_home, "bin")
      if (!dir.exists(cargo_bin_dir)) next
      cargo_cmd <- file.path(cargo_bin_dir, paste0("cargo", ifelse(windows, ".exe", "")))
      rustup_home <- cargo_home <- file.path(prefix_dir, "rustup")
      vars <- c(environment_variables,
        PATH = paste0(Sys.getenv("PATH"), .Platform$path.sep, cargo_bin_dir),
        RUSTUP_HOME = rustup_home
      )
      vars <- if (leave_no_trace) c(vars, CARGO_HOME_ORIGINAL = cargo_home) else c(vars, CARGO_HOME = cargo_home)
      status <- run_engine("R_CARGO_RUN_CACHE", cargo_cmd, vars, TRUE)
      if ((!is.null(status)) && (!is.numeric(status) || (status == 0))) {
        return(status)
      }
    }
  }
  msg("No suitable version of Cargo was found.\nOne solution is to run 'cargo::install()'.\n---\n")
  1
}

# An enhancement of the system2 function which sets environment variables better.
system3 <- function(..., env = character()) {
  if (length(env) > 0) {
    names <- names(env)
    original_env <- sapply(names, function(x) Sys.getenv(x, "<unset>"))
    tmp <- original_env != "<unset>"
    to_restore <- original_env[tmp]
    to_unset <- names(original_env[!tmp])
    tmp <- !is.na(env)
    if (sum(tmp) > 0) do.call(Sys.setenv, as.list(env[tmp]))
    if (sum(!tmp) > 0) Sys.unsetenv(names(env[!tmp]))
    on.exit({
      if (length(to_restore) > 0) do.call(Sys.setenv, as.list(to_restore))
      Sys.unsetenv(to_unset)
    })
  }
  system2(...)
}

mk_rustflags <- function(...) {
  args <- c(...)
  if (is.null(args)) {
    list()
  } else {
    x <- if (any(is.na(args)) || (length(args) == 0)) {
      NA
    } else {
      paste(args, collapse = rawToChar(as.raw(strtoi("1f", base = 16L))))
    }
    list(CARGO_ENCODED_RUSTFLAGS = x)
  }
}

cache_dir <- function() tools::R_user_dir("cargo", "cache")

target <- function(sysname = Sys.info()[["sysname"]], arch = R.version$arch) {
  if (sysname == "Linux") {
    if (arch == "aarch64") {
      "aarch64-unknown-linux-gnu"
    } else if (arch == "x86_64") {
      "x86_64-unknown-linux-gnu"
    } else {
      stop("Cannot determine target.")
    }
  } else if (sysname == "Darwin") {
    if (arch == "aarch64") {
      "aarch64-apple-darwin"
    } else if (arch == "x86_64") {
      "x86_64-apple-darwin"
    } else {
      stop("Cannot determine target.")
    }
  } else if (sysname == "Windows") {
    if (arch == "x86_64") {
      "x86_64-pc-windows-gnu"
    } else {
      stop("Cannot determine target.")
    }
  } else {
    stop("Cannot determine target.")
  }
}

targets <- function() {
  c(
    target("Linux", "aarch64"), target("Linux", "x86_64"),
    target("Darwin", "aarch64"), target("Darwin", "x86_64"),
    target("Windows", "x86_64")
  )
}
