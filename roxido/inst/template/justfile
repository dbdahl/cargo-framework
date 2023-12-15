install target = ".":
  R CMD INSTALL {{target}}

build:
  R CMD build .

document:
  Rscript -e "roxygen2::roxygenise()"

cran:
  Rscript -e "roxido::build_for_cran()"

deploy host = "": cran (install `just srcpkg`)
  #!/usr/bin/env sh
  if test "{{host}}" != ""
  then
    srcpkg=`Rscript -e "a <- read.dcf('DESCRIPTION'); cat(a[,'Package'],'_',a[,'Version'],'.tar.gz',sep='')"`
    scp "$srcpkg" {{host}}:.
    ssh {{host}} R CMD INSTALL "$srcpkg"
  fi

api:
  Rscript -e "roxido::api_documentation()"

srcpkg:
  Rscript -e "a <- read.dcf('DESCRIPTION'); cat(a[,'Package'],'_',a[,'Version'],'.tar.gz',sep='')"

