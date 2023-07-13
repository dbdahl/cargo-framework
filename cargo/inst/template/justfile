build:
  R CMD build .

install target=default_target:
  R CMD INSTALL {{target}}

document:
  Rscript -e "roxygen2::roxygenise()"

cran:
  Rscript -e "cargo::build_for_cran()"

api:
  Rscript -e "cargo::api_documentation()"

default_target := "."
