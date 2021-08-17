# The Cargo Framework

The paper [*Writing R Extensions in Rust*](https://arxiv.org/pdf/2108.07179.pdf)
complements [*Writing R
Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html), the official
guide for writing R extensions, for those interested in developing
[R](https://www.r-project.org/) packages using
[Rust](https://www.rust-lang.org/). It highlights idiosyncrasies of
[R](https://www.r-project.org/) and [Rust](https://www.rust-lang.org/) that must
be addressed by any integration and describes how to develop
[Rust](https://www.rust-lang.org/)-based packages which comply with the [CRAN
Repository Policy](https://cran.r-project.org/web/packages/policies.html).  The
[paper](https://arxiv.org/pdf/2108.07179.pdf) introduces the cargo framework, a
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

For usage information, please read the paper [*Writing R Extensions in
Rust*](https://arxiv.org/pdf/2108.07179.pdf).

