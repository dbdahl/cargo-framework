# The Cargo Framework

The paper [*Writing R Extensions in Rust*](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
complements [*Writing R Extensions*](https://cran.r-project.org/doc/manuals/R-exts.html)
(the official guide for writing R extensions) for those interested in developing
[R](https://www.r-project.org/) packages using [Rust](https://www.rust-lang.org/).
It highlights idiosyncrasies of [R](https://www.r-project.org/) and
[Rust](https://www.rust-lang.org/) that must be addressed by any integration and
describes how to develop [Rust](https://www.rust-lang.org/)-based packages which
comply with the [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html).
The [paper](https://raw.githubusercontent.com/dbdahl/cargo-framework/main/cargo/inst/doc/Writing_R_Extensions_in_Rust.pdf)
introduces the cargo framework, a transparent
[Rust](https://www.rust-lang.org/)-based API which wraps commonly-used parts of
[R](https://www.r-project.org/)'s API with minimal overhead and allows a
programmer to easily add additional wrappers.

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

