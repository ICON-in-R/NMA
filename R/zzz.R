
#' @export
#'
.onLoad <- function(libname, pkgname) {

  SYS <- .Platform$OS.type
  PROG <- "openBugs"
  
  if (SYS == "MAC") {
    fileSep <- "/"
    newSavePlot <-
      function(file)
        quartz.save(file = file, type = "pdf")
  }
  
  if (SYS == "windows") {
    fileSep <- "\\"
    newSavePlot <-
      function(file)
        savePlot(filename = file, type = "pdf")
  }
  
  if (PROG == "openBugs") {
    newBugs <- function(...)
      bugs(program = "openbugs", ...)
  }
  
  if (PROG == "winBugs") {
    newBugs <- function(...)
      bugs(program = "winbugs", ...)
  }
  
  if (PROG == "JAGS") {
    newBugs <- function(...)
      jags(...)
  }
  
  invisible()
}

