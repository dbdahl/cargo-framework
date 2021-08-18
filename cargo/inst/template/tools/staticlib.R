source("../tools/cargo.R")

target_args <- if ( .Platform$OS.type == "windows" ) c("--target", target()) else NULL

if ( run("build", target_args, "--release", "--manifest-path", "rustlib/Cargo.toml") ) {

  if ( ! is.null(target_args) ) {
    args <- commandArgs(TRUE)
    lib_dir_template <- args[1]
    statlib          <- args[2]
    dir.create(dirname(statlib), showWarnings=FALSE, recursive=TRUE)
    file.copy(file.path(gsub("___",target_args[2],lib_dir_template),basename(statlib)),statlib)
  }

} else {

##
## The "run(...)" above may fail to build the static library because it cannot
## find a sufficient version of the Rust toolchain. As a fallback, R can
## download it from some place where you host it yourself. Having several
## download locations provides robustness. The ${name}, ${version} and ${target}
## values are automatically determined, but you will need to modify the URL to
## fit your situation. For example, see below. These static libraries can built
## by the cargo::cross_compile function.
##
#  download_staticlib(
#    "https://r.ddahl.org/staticlib/${name}_${version}/${target}.tar.gz"
#    ,
#    "https://dahl.byu.edu/rrepository/staticlib/${name}_${version}/${target}.tar.gz"
#  )

}
