library(testpkg)

while (TRUE) {
  tryCatch(convolve2(c(1,2), list()), error=function(e) {})
}

