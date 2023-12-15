# The Cargo Framework

The paper
[*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf) 
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
[cargo](https://cran.r-project.org/package=cargo) package on
[CRAN](https://cran.r-project.org/).

## Installation

Install using the 
[remotes](https://cran.r-project.org/package=remotes) package:

```r
install.packages("remotes")
remotes::install_github("dbdahl/cargo-framework/cargo")
```

## Usage

For usage information, please see the paper
[*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf).

The [rustdoc](https://docs.rs/roxido/) for the [roxido](https://crates.io/crates/roxido) crate
documents the `Rval` structure and its functions.

## Examples

The following are [Rust](https://www.rust-lang.org/)-based packages on [CRAN](https://cran.r-project.org/)
that were developed using this framework.

+ [salso](https://cran.r-project.org/package=salso)
+ [caviarpd](https://cran.r-project.org/package=caviarpd)
+ [fangs](https://cran.r-project.org/package=fangs)

## Getting Started

Please read the paper
[*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
for full details but, to get a taste, consider the follow...

Setting up your environment is easy.

```r
cargo::install()
```

Make a new package named, for example, `foo`:

```r
cargo::new_package("foo")
```

Install your new package and try out its `myrnorm` function:

```r
install.packages("foo", repos=NULL, type="source")
set.seed(1L)
foo::myrnorm(5, 0, 1)
```
```
[1] -0.6264538  0.1836433 -0.8356286  1.5952808  0.3295078
```

Now start hacking away on your new Rust-based package.  In particular, some of
the more interesting files are listed below.  See especially `src/rust/src/lib.rs`.

```
foo
├── DESCRIPTION
├── R
│   └── myrnorm.R
└── src
    └── rust
        ├── roxido ...
        └── src
            └── lib.rs
```

You can browse the documentation of the API for the cargo framework:

```r
cargo::api_documentation("foo")
```

And, you can extend the framework by editing `foo/src/rust/roxido/src/r.rs`.

Finally, you can also embed Rust code directly in your R scripts:

```r
euclidean_norm <- cargo::rust_fn(x, '
    let ss = x.slice_double().unwrap().iter().fold(0.0, |s,&z| s + z*z);
    Rval::new(ss.sqrt(), pc)
')

euclidean_norm(rnorm(10))
```
```
[1] 3.29764
```

Notice that if you redefine the `euclidean_norm` function, the second
complication is much faster.

Again, for full details, please read the paper
[*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf).

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
