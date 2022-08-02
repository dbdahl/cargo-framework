a <- available.packages()
pkgnames <- a[,"Package"]
dir.create("packages",showWarnings=FALSE)
parallel::mclapply(pkgnames, download.packages, destdir="packages", mc.cores = 4L)
