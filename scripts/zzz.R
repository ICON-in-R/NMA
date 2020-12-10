
library("meta")
library("reshape")
library("coda")
library("boot")
library("tcltk")
library("sqldf")
library("R2OpenBUGS")
library("R2WinBUGS")
#library("R2jags")
library("e1071")
library("gplots")
library("gtools")
library("grDevices")
library("sna")
library(network)


SYS <- "WIN"
PROG <- "openBugs"
#RUN <- TRUE

if (SYS == "MAC") {
  fileSep <- "/"
  newSavePlot <-
    function(file) {
      return(quartz.save(file = file, type = "pdf"))
    }
}

if (SYS == "WIN") {
  fileSep <- "\\"
  newSavePlot <-
    function(file) {
      return(savePlot(filename = file, type = "pdf"))
    }
}

BASEPROB <- NA


if (PROG == "openBugs") {
  newBugs <- function(...) {
    return(bugs(program = "openbugs", ...))
  }
}

if (PROG == "winBugs") {
  newBugs <- function(...) {
    return(bugs(program = "winbugs", ...))
  }
}


if (PROG == "JAGS") {
  newBugs <- function(...) {
    return(jags(...))
  }
}

