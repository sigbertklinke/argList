do <- new.env()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("argList ", utils::packageVersion("argList"),': see the package vignette with `vignette("argList")`'))
}
