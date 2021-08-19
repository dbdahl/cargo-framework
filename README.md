# The Cargo Framework

The paper
[*Writing R Extensions in Rust*]( https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf) 
complements
[*Writing R Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html)
(the official guide for writing R extensions) for those interested in developing
[R](https://www.r-project.org/) packages using
[Rust](https://www.rust-lang.org/). It highlights idiosyncrasies of
[R](https://www.r-project.org/) and [Rust](https://www.rust-lang.org/) that must
be addressed by any integration and describes how to develop
[Rust](https://www.rust-lang.org/)-based packages which comply with the [CRAN
Repository Policy](https://cran.r-project.org/web/packages/policies.html).  The
[paper]( https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf) 
introduces the cargo framework, a
transparent [Rust](https://www.rust-lang.org/)-based API which wraps
commonly-used parts of [R](https://www.r-project.org/)'s API with minimal
overhead and allows a programmer to easily add additional wrappers.

This repository hosts the source of the
[cargo](https://cran.r-project.org/package=cargo) package hosted on
[CRAN](https://cran.r-project.org/).

## Installation

Install the released version of the
[cargo](https://cran.r-project.org/package=cargo) package from
[CRAN](https://cran.r-project.org/):

```r
install.packages("cargo")
```

You can also install the development version using the
[remotes](https://cran.r-project.org/package=remotes) package:

```r
remotes::install_github("dbdahl/cargo-framework/cargo")
```

## Usage

For usage information, please see the paper
[*Writing R Extensions in Rust*]( https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf).

## Examples

The following are [Rust](https://www.rust-lang.org/)-based packages on [CRAN](https://cran.r-project.org/)
that were developed using this framework.

+ [salso](https://cran.r-project.org/package=salso)
+ [caviarpd](https://cran.r-project.org/package=caviarpd)

## Citation

To cite in publications, please use

```
David B. Dahl (2021), Writing R Extensions in Rust, arXiv:2108.07179 [cs.PL], URL https://arxiv.org/abs/2108.07179.
```

A BibTeX entry for LaTeX users is

```
  @Misc{,
    title = {Writing R Extensions in Rust},
    author = {David B. Dahl},
    year = {2021},
    eprint = {2108.07179},
    archiveprefix = {arXiv},
    primaryclass = {cs.PL},
  }
```

# Related Projects

+ [extendr](https://github.com/extendr/extendr)
+ [Rust in R](https://github.com/r-rust)
