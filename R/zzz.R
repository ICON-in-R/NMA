
#' @export
#'
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Welcome to NMA.")
  options(jags.pb = "text")
  invisible()
}

