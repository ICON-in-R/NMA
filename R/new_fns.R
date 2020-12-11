
#'
newSavePlot <- function() {
  
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


#'
newBugs <- function(PROG = "openBugs", ...) {
  
  if (PROG == "openBugs") {
    return(
      function(...)
        bugs(program = "openbugs", ...))
  }
  
  if (PROG == "winBugs") {
    return(
      function(...)
        bugs(program = "winbugs", ...))
  }
  
  if (PROG == "JAGS") {
    return(
      function(...)
        jags(...))
  }
}

