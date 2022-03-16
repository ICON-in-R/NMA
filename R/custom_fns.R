
#'
customSavePlot <- function() {
  
  SYS <- .Platform$OS.type
  
  if (SYS == "MAC") {
    fileSep <- "/"
    return(
      function(file)
        quartz.save(file = file, type = "pdf"))
  }
  
  if (SYS == "windows") {
    fileSep <- "\\"
    return(
      function(file)
        savePlot(filename = file, type = "pdf"))
  }
}


#' @importFrom R2WinBUGS bugs
#' @importFrom R2jags jags
#'
customBugs <- function(PROG = "openbugs", ...) {
  
  PROG <- tolower(PROG)
  
  if (PROG == "openbugs") {
    return(
      function(...)
        bugs(program = "openbugs", ...))
  }
  
  if (PROG == "winbugs") {
    return(
      function(...)
        bugs(program = "winbugs", ...))
  }
  
  if (PROG == "jags") {
    return(
      function(...)
        R2jags::jags(...))
  }
}

