###### 3. Load NMA functions ######


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


codeVariable <- function(var, codeList) {
  codedVariable <- match(var, codeList)
  return(codedVariable)
}


createFolders <- function(folder, ...) {
  subFolders <- list(...)
  
  ##TODO: why not just use inbuilt R commands?
  
  if (SYS == "WIN") {
    if (!file.exists(folder))
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste("mkdir ", folder, sep = "")
      ))
    for (subFolder in subFolders) {
      if (!file.exists(paste(folder, fileSep, subFolder, sep = "")))
        system(paste(
          Sys.getenv("COMSPEC"),
          "/c",
          paste("mkdir ", folder, fileSep, subFolder, sep = "")
        ))
    }
  }
  
  if (SYS == "MAC") {
    if (!file.exists(folder))
      system(paste("mkdir ", folder, sep = ""))
    for (subFolder in subFolders) {
      if (!file.exists(paste(folder, fileSep, subFolder, sep = "")))
        system(paste("mkdir ", folder, fileSep, subFolder, sep = ""))
    }
  }
  
}

## Pairwise Table
pairwiseTable <- function(sims) {
  txList <- colnames(sims)
  nTx <- length(txList)
  
  pairwiseTable <- matrix(NA, nTx, nTx)
  for (tx1 in 1:nTx) {
    for (tx2 in 1:nTx) {
      pairwiseTable[tx1, tx2] <-
        paste(
          round(exp(median(
            sims[, tx2] - sims[, tx1], na.rm = TRUE
          )), 2),
          " (",
          paste(round(exp(
            quantile(sims[, tx2] - sims[, tx1], c(0.025, 0.975), na.rm = TRUE)
          ), 2), collapse = ",", sep = ""),
          ")",
          collapse = "",
          sep = ""
        )
      
    }
  }
  
  colnames(pairwiseTable) <- txList
  rownames(pairwiseTable) <- txList
  
  pairwiseTable <- t(pairwiseTable)
  
  return(pairwiseTable)
}


## Treatment Ranking Plot
rankProbPlot <- function(sims) {
  nRanks <- ncol(sims)
  
  sims <- sims[, order(apply(sims, 2, mean), decreasing = TRUE)]
  rankSummary <- numeric()
  ranks <- apply(sims, 1, rank, ties.method = "random")
  
  for (i in 1:ncol(sims)) {
    rankSummary <- c(rankSummary, apply(ranks == i, 1, mean))
  }
  
  Ranking <- rep(1:ncol(sims), rep(ncol(sims), ncol(sims)))
  txList <- colnames(sims)
  Treatment <- rep(txList, ncol(sims))
  rankSummary <- as.numeric(round(rankSummary, 2))
  
  if (length(txList) > 12) {
    txtSize <- 0.35
  } else{
    txtSize <- 0.55
  }
  
  main.txt <-
    paste(
      label,
      "Rank based on treatment effect",
      "Rank 1: Most effective treatment (Smallest hazard ratio)",
      sep = "\n"
    )
  
  par(mar = c(2, 2, 3, 2))
  
  balloonplot(
    x = Ranking,
    y = Treatment,
    z = rankSummary,
    rowmar = 4,
    colmar = 1,
    label.lines = FALSE,
    sort = FALSE,
    show.zeros = TRUE,
    show.margins = FALSE,
    cum.margins = FALSE,
    main = mtext(text = main.txt, cex = 0.6),
    text.size = txtSize,
    label.size = txtSize
  )
  
}

summStat <- function(x) {
  y <- c(mean(x), quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE))
  names(y)[1] <- "mean"
  return(y)
}

## NMA Forest


codeVariable <- function(var, codeList) {
  codedVariable <- match(var, codeList)
  return(codedVariable)
}

codeData <- function(subData, refTx) {
  subData <<- subData
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<- unique(sort(c(subData$tx, subData$base)))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<- unique(c(refTx, sort(c(
      subData$tx, subData$base
    ))))
  } else{
    refTx <<- txList[1]
  }
  
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx , codeList = txList)
  subData$Lbase  <-
    codeVariable(var = subData$base , codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  
  ## code studies
  studyList <- unique(subData$study)
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  nStudies <<- max(subData$Lstudy , 2)
  nObs <<- length(subData$Lstudy)
  
  return(subData)
}

codeDataBin <- function(subData, subDataBin, refTx) {
  subData <<- subData
  subDataBin <<- subDataBin
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<-
    unique(sort(c(
      subData$tx, subData$base, subDataBin$tx, subDataBin$base
    )))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<-
      unique(c(refTx, sort(
        c(subData$tx, subData$base, subDataBin$tx, subDataBin$base)
      )))
  } else{
    refTx <<- txList[1]
  }
  
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx , codeList = txList)
  subData$Lbase  <-
    codeVariable(var = subData$base , codeList = txList)
  subDataBin$Btx  <-
    codeVariable(var = subDataBin$tx , codeList = txList)
  subDataBin$Bbase  <-
    codeVariable(var = subDataBin$base , codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx),]
  
  ## code studies
  studyList <- unique(sort(c(subData$study, subDataBin$study)))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataBin$Bstudy <-
    codeVariable(var = subDataBin$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataBin$Bstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  BnObs <<- length(subDataBin$Bstudy)
  
  return(list(subData, subDataBin))
}

codeDataMed <- function(subData, subDataMed, refTx) {
  subData <<- subData
  subDataMed <<- subDataMed
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<-
    unique(sort(c(
      subData$tx, subData$base, subDataMed$tx, subDataMed$base
    )))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<-
      unique(c(refTx, sort(
        c(subData$tx, subData$base, subDataMed$tx, subDataMed$base)
      )))
  } else{
    refTx <<- txList[1]
  }
  
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx , codeList = txList)
  subData$Lbase  <-
    codeVariable(var = subData$base , codeList = txList)
  subDataMed$mediantx  <-
    codeVariable(var = subDataMed$tx , codeList = txList)
  subDataMed$medianbase  <-
    codeVariable(var = subDataMed$base , codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx),]
  
  ## code studies
  studyList <- unique(sort(c(subData$study, subDataMed$study)))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataMed$medianstudy <-
    codeVariable(var = subDataMed$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataMed$medianstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  medianNObs <<- length(subDataMed$medianstudy)
  
  return(list(subData, subDataMed))
}



# Function to set up results file for multiple parameters monitored
resultsFileSetUp <- function(resultsFile,
                             effectParam,
                             dummy,
                             dummyOR,
                             colEff) {
  eff <-
    round(resultsFile[grep(paste("^", effectParam, sep = ""),
                           rownames(resultsFile)), c(1, 5, 2, 3, 7, 8)], 2)
  eff <- eff[!grepl(paste("^", "dev", sep = ""), rownames(eff)),]
  #eff <- eff[!grepl(paste("^","Ldev",sep=""),rownames(eff)),]
  
  if (length(txList) != nrow(eff)) {
    if (effectParam != "hr") {
      eff <- rbind(dummy, eff)
    } else{
      eff <- rbind(dummyOR, eff)
    }
  }
  colnames(eff) <- paste(colEff)
  rownames(eff)  <- txList
  return(eff)
}


