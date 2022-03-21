
#'
customSavePlot <- function() {
  
  SYS <- .Platform$OS.type
  
  if (SYS == "MAC") {
    return(
      function(file)
        quartz.save(file = file, type = "pdf"))
  }
  
  if (SYS == "windows") {
    return(
      function(file)
        savePlot(filename = file, type = "pdf"))
  }
}


#' @importFrom R2WinBUGS bugs
#' @importFrom R2OpenBUGS bugs
#' @importFrom R2jags jags
#'
customBugs <- function(PROG = "openbugs", ...) {
  
  PROG <- tolower(PROG)
  
  if (PROG == "openbugs") {
    return(
      function(...)
        R2OpenBUGS::bugs(...))
  }
  
  if (PROG == "winbugs") {
    return(
      function(...)
        R2WinBUGS::bugs(...))
  }
  
  if (PROG == "jags") {
    return(
      function(...)
        R2jags::jags(...))
  }
}

