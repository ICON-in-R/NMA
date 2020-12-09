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

#library("Rgraphviz")



SYS <- "WIN"
PROG <- "openBugs"
#RUN <- TRUE

#if(SYS=="MAC") fileSep <- "/"
#if(SYS=="WIN") fileSep <- "\\"

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
    #return(bugs(OpenBUGS.pgm ="C:\\Program Files\\OpenBUGS\\OpenBUGS322\\OpenBUGS.exe",...))
    #return(bugs(OpenBUGS.pgm ="C:\\Program Files (x86)\\OpenBUGS\\OpenBUGS322\\OpenBUGS.exe",...))
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

## Study Network Diagram
varPlot <-
  function(xVar,
           yVar ,
           showYLab = TRUE,
           showXLab = TRUE,
           pointText = NA,
           joinY = TRUE ,
           txList = NA,
           folder,
           label,
           cex.axis = 0.2,
           ...) {
    xVar <<- xVar
    yVar <<- yVar
    showYLab <<- showYLab
    showXLab <<- showXLab
    joinY <<- joinY
    txList <<- txList
    folder <<- folder
    label <<- label
    cex.axis <<- cex.axis
    pointText <<- pointText
    
    xVar <- as.character(xVar)
    #y <- as.character(y)
    numX <- as.numeric(factor(xVar))
    numY <- as.numeric(factor(yVar, levels = unique(yVar)))
    
    if (!is.na(txList[1]))
      numX <- match(xVar, txList)
    if (is.na(txList[1]))
      txList <- unique(xVar)
    
    byVar <- TRUE
    
    plot(
      numX,
      numY,
      type = "n",
      xlab = " ",
      ylab = " ",
      bty = "n",
      yaxt = "n",
      xaxt = "n"
    )
    for (currY in unique(yVar)) {
      #  points(x=numX[yVar==currY],y=numY[yVar==currY],cex=0.2,col="black",pch=19)
      # text(x=numX[yVar==currY],y=numY[yVar==currY],labels=subData$r[yVar==currY],cex=0.7)
      if (joinY)
        lines(
          x = numX[yVar == currY],
          y = numY[yVar == currY],
          cex = 0.35,
          col = "grey",
          pch = 19,
          lwd = 3
        )
    }
    
    abline(v = seq(along = unique(xVar)), col = "light grey")
    
    for (currY in unique(yVar)) {
      if (is.na(pointText)) {
        points(
          x = numX[yVar == currY],
          y = numY[yVar == currY],
          cex = 0.8,
          col = "black",
          pch = 19
        )
      } else {
        text(
          x = numX[yVar == currY],
          y = numY[yVar == currY],
          labels = pointText[yVar == currY],
          cex = cex.axis
        )
      }
    }
    
    
    if (showXLab)
      axis(
        1,
        at = 1:max(numX),
        lab = txList,
        las = 2,
        lwd = 0.35,
        cex.axis = 0.5,
        cex = 2,
        cex.lab = 2
      )
    if (showYLab)
      axis(
        2,
        at = 1:max(numY),
        lab = levels(factor(yVar, levels = unique(yVar))),
        lwd = 0.35,
        las = 1,
        cex.axis = 0.5,
        cex = 0.5,
        cex.lab = 0.5
      )
    mtext(
      text = label,
      side = 3,
      cex = 0.8,
      padj = -1
    )
    
  }


#a <- graph.adjacency(networkData/networkData)

## Pairwise Table
pairwiseTable <- function(sims) {
  txList <- colnames(sims)
  nTx <- length(txList)
  
  pairwiseTable <- matrix(NA, nTx, nTx)
  for (tx1 in 1:nTx) {
    for (tx2 in 1:nTx) {
      #if (lg==TRUE){
      #  pairwiseTable[tx1,tx2] <- paste(round(median(sims[,tx2]/sims[,tx1], na.rm = TRUE),2),"(",paste(round(quantile(sims[,tx2]/sims[,tx1],c(0.025,0.975), na.rm = TRUE),2),collapse=",",sep=""),")",collapse="",sep="")
      #}else{
      #  pairwiseTable[tx1,tx2] <- paste(round(mean(sims[,tx2]-sims[,tx1], na.rm = TRUE),2),"(",paste(round(quantile(sims[,tx2]-sims[,tx1],c(0.025,0.975), na.rm = TRUE),2),collapse=",",sep=""),")",collapse="",sep="")
      #}
      
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

txEffectPlot <- function(sims) {
  plotSims <- sims
  
  txListSims <- colnames(plotSims)
  
  if ((preRefTx %in% txListSims) == TRUE &
      (!is.na(preRefTx)) == TRUE) {
    if (lg == FALSE) {
      plotSims <- plotSims - plotSims[, preRefTx]
    } else{
      plotSims <- plotSims - plotSims[, preRefTx]
    }
  }
  
  if ((preRefTx %in% txListSims) == FALSE |
      (!is.na(preRefTx)) == FALSE) {
    plotSims <- plotSims - plotSims[, refTx]
  }
  
  plotResults <- round(exp(t(apply(
    plotSims, 2, summStat
  ))), 2)
  
  plotResults <-
    plotResults[order(plotResults[, 2], decreasing = TRUE),]
  plotResults[plotResults == 0] <- 0.001
  
  txList <- rownames(plotResults)
  nTx <- length(txList)
  
  layout(cbind(1, 2), widths = c(2, 1))
  par(mar = c(5, 12, 2, 1), cex = 0.8)
  
  
  plot(
    plotResults[, 2],
    1:nTx,
    ylim = c(0.75, nTx + 0.25),
    xlim = (range(plotResults[, 3], plotResults[, 4])),
    yaxt = "n",
    xaxt = "n",
    main = label,
    cex.main = 0.6,
    xlab = c(endpoint, "Median hazard ratio (95% CrI)"),
    ylab = " ",
    pch = 19,
    type = "n",
    log = "x",
    cex = 0.8
  )
  
  axis(
    1,
    at = c(round(0.5 ^ seq(5:1), 3), 1, round(1 / (0.5 ^ seq(1:4)))),
    label = c(round(0.5 ^ seq(5:1), 3), 1, round(1 / (0.5 ^ seq(1:4)))),
    cex.axis = 0.8
  )
  abline(v = 1, col = "grey")
  
  axis(
    2,
    at = 1:nTx,
    labels = txList,
    las = 2,
    cex.axis = 0.8,
    cex = 0.8
  )
  
  
  for (ii in 1:nTx) {
    lines(c(plotResults[ii, 3], plotResults[ii, 4]),
          c(ii, ii),
          col = "black",
          lwd = 2)
  }
  
  for (ii in 1:nTx) {
    points(
      plotResults[ii, 2],
      ii,
      pch = 21,
      cex = 2,
      bg = "black",
      col = "white",
      lwd = 2
    )
  }
  
  par(mar = c(5, 0, 2, 0))
  plot(
    rep(0, nTx),
    1:nTx,
    xlim = c(0, 1),
    ylim = c(0.75, nTx + 0.25),
    yaxt = "n",
    type = "n",
    bty = "n",
    xaxt = "n",
    ylab = " ",
    xlab = " "
  )
  
  for (ii in 1:nTx) {
    if (plotResults[ii, 2] == plotResults[ii, 3] &
        plotResults[ii, 2] == plotResults[ii, 3]) {
      text(0,
           ii,
           "Reference Treatment",
           pos = 4,
           cex = 0.8)
    } else{
      text(
        0,
        ii,
        paste(
          plotResults[ii, 2],
          " (",
          plotResults[ii, 3],
          " to ",
          plotResults[ii, 4],
          ")",
          sep = ""
        ),
        pos = 4,
        cex = 0.8
      )
    }
  }
  
}

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

codeDataBinMed <- function(subData, subDataBin, subDataMed, refTx) {
  subData <<- subData
  subDataMed <<- subDataMed
  subDataBin <<- subDataBin
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<-
    unique(sort(
      c(
        subData$tx,
        subData$base,
        subDataMed$tx,
        subDataMed$base,
        subDataBin$tx,
        subDataBin$base
      )
    ))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<-
      unique(c(refTx, sort(
        c(
          subData$tx,
          subData$base,
          subDataMed$tx,
          subDataMed$base,
          subDataBin$tx,
          subDataBin$base
        )
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
  subDataBin$Btx  <-
    codeVariable(var = subDataBin$tx , codeList = txList)
  subDataBin$Bbase  <-
    codeVariable(var = subDataBin$base , codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx),]
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx),]
  
  ## code studies
  studyList <-
    unique(sort(c(
      subData$study, subDataMed$study, subDataBin$study
    )))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataMed$medianstudy <-
    codeVariable(var = subDataMed$study, codeList = studyList)
  subDataBin$Bstudy <-
    codeVariable(var = subDataBin$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataMed$medianstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  medianNObs <<- length(subDataMed$medianstudy)
  BnObs <<- length(subDataBin$Bstudy)
  
  return(list(subData, subDataMed, subDataBin))
}


#'
setupData <-
  function(subData,
           subDataBin,
           binData,
           subDataMed,
           medData,
           random,
           refTx) {
    
    if (binData == FALSE & medData == FALSE) {
      subData <- codeData(subData = subData, refTx = refTx)
      
      LstudyT <- subData$Lstudy
      LtxT <- subData$Ltx
      LbaseT <- subData$Lbase
      LmeanT <- subData$Lmean
      LseT <- subData$Lse
      multiT <- subData$multi
      
      LnObsT <- nrow(subData)
      nTxT <- length(txList)
      nStudiesT <- max(subData$Lstudy)
      
      bugsData <<- list(
        Lstudy = LstudyT,
        Ltx = LtxT,
        Lbase = LbaseT,
        Lmean = LmeanT,
        Lse = LseT,
        multi = multiT,
        LnObs = LnObsT,
        nTx = nTxT,
        nStudies = nStudiesT
      )
      
      if (!random) {
        inits <-
          function() {
            list(beta = c(NA, rnorm(nTx - 1, 0, 2)),
                 alpha = rnorm(nStudies))
          }
      } else {
        inits <-
          function() {
            list(
              beta = c(NA, rnorm(nTx - 1, 0, 2)),
              sd = 0.1,
              alpha = rnorm(nStudies)
            )
          }
      }
      
      subData <<- subData
      inits <<- inits
    }
    
    if (binData & medData) {
      subDatalist <-
        codeDataBin(subData = subData,
                    subDataBin = subDataBin,
                    refTx = refTx)
      subData <- as.data.frame(subDatalist[1])
      subDataBin <- as.data.frame(subDatalist[2])
      
      LstudyT <- subData$Lstudy
      LtxT <- subData$Ltx
      LbaseT <- subData$Lbase
      LmeanT <- subData$Lmean
      LseT <- subData$Lse
      multiT <- subData$multi
      
      BstudyT <- subDataBin$Bstudy
      BtxT <- subDataBin$Btx
      BbaseT <- subDataBin$Bbase
      BrT <- subDataBin$BinR
      BnT <- subDataBin$BinN
      
      LnObsT <- nrow(subData)
      BnObsT <- nrow(subDataBin)
      
      nTxT <- length(txList)
      nStudiesT <- max(subData$Lstudy, subDataBin$Bstudy)
      
      bugsData <<- list(
        Lstudy = LstudyT,
        Ltx = LtxT,
        Lbase = LbaseT,
        Bstudy = BstudyT,
        Btx = BtxT,
        Bbase = BbaseT,
        Lmean = LmeanT,
        Lse = LseT,
        multi = multiT,
        LnObs = LnObsT,
        Bn = BnT,
        Br = BrT,
        BnObs = BnObsT,
        nTx = nTxT,
        nStudies = nStudiesT)
      
      if (!random)
        inits <-
        function() {
          list(beta = c(NA, rnorm(nTx - 1, 0, 2)),
               alpha = rnorm(nStudies))
        }
      if (random)
        inits <-
        function() {
          list(
            beta = c(NA, rnorm(nTx - 1, 0, 2)),
            sd = 0.1,
            alpha = rnorm(nStudies))
        }
      
      subData <<- subData
      subDataBin <<- subDataBin
      
      inits <<- inits
    }
    
    if (!binData & medData) {
      subDatalist <-
        codeDataMed(subData = subData,
                    subDataMed = subDataMed,
                    refTx = refTx)
      subData <- as.data.frame(subDatalist[1])
      subDataMed <- as.data.frame(subDatalist[2])
      
      LstudyT <- subData$Lstudy
      LtxT <- subData$Ltx
      LbaseT <- subData$Lbase
      LmeanT <- subData$Lmean
      LseT <- subData$Lse
      multiT <- subData$multi
      
      medianStudyT <- subDataMed$medianstudy
      medianTxT <- subDataMed$mediantx
      medianBaseT <- subDataMed$medianbase
      medianRT <- subDataMed$medR
      medianNT <- subDataMed$medN
      medianT <- subDataMed$median
      
      LnObsT <- nrow(subData)
      medianNObsT <- nrow(subDataMed)
      
      nTxT <- length(txList)
      nStudiesT <- max(subData$Lstudy, subDataMed$mediantudy)
      
      bugsData <<- list(
        Lstudy = LstudyT,
        Ltx = LtxT,
        Lbase = LbaseT,
        medianStudy = medianStudyT,
        medianTx = medianTxT ,
        medianBase = medianBaseT,
        Lmean = LmeanT,
        Lse = LseT,
        multi = multiT,
        LnObs = LnObsT,
        medianN = medianNT,
        medianR = medianRT,
        medianNObs = medianNObsT,
        median = medianT,
        nTx = nTxT,
        nStudies = nStudiesT
      )
      
      if (!random)
        inits <-
        function() {
          list(beta = c(NA, rnorm(nTx - 1, 0, 2)),
               alpha = rnorm(nStudies))
        }
      if (random)
        inits <-
        function() {
          list(
            beta = c(NA, rnorm(nTx - 1, 0, 2)),
            sd = 0.1,
            alpha = rnorm(nStudies)
          )
        }
      
      subData <<- subData
      subDataMed <<- subDataMed
      
      inits <<- inits
    }
    
    if (binData == TRUE & medData == TRUE) {
      subDatalist <-
        codeDataBinMed(
          subData = subData,
          subDataMed = subDataMed,
          subDataBin,
          refTx = refTx
        )
      subData <- as.data.frame(subDatalist[1])
      subDataMed <- as.data.frame(subDatalist[2])
      subDataBin <- as.data.frame(subDatalist[3])
      
      LstudyT <- subData$Lstudy
      LtxT <- subData$Ltx
      LbaseT <- subData$Lbase
      LmeanT <- subData$Lmean
      LseT <- subData$Lse
      multiT <- subData$multi
      
      medianStudyT <- subDataMed$medianstudy
      medianTxT <- subDataMed$mediantx
      medianBaseT <- subDataMed$medianbase
      medianRT <- subDataMed$medR
      medianNT <- subDataMed$medN
      medianT <- subDataMed$median
      
      BstudyT <- subDataBin$Bstudy
      BtxT <- subDataBin$Btx
      BbaseT <- subDataBin$Bbase
      BrT <- subDataBin$BinR
      BnT <- subDataBin$BinN
      
      LnObsT <- nrow(subData)
      medianNObsT <- nrow(subDataMed)
      BnObsT <- nrow(subDataBin)
      
      nTxT <- length(txList)
      nStudiesT <-
        max(subData$Lstudy,
            subDataMed$medianstudy,
            subDataBin$Bstudy)
      
      bugsData <<- list(
        Lstudy = LstudyT,
        Ltx = LtxT,
        Lbase = LbaseT,
        medianStudy = medianStudyT,
        medianTx = medianTxT ,
        medianBase = medianBaseT,
        Bstudy = BstudyT,
        Btx = BtxT,
        Bbase = BbaseT,
        Lmean = LmeanT,
        Lse = LseT,
        multi = multiT,
        LnObs = LnObsT,
        medianN = medianNT,
        medianR = medianRT,
        medianNObs = medianNObsT,
        median = medianT,
        Bn = BnT,
        Br = BrT,
        BnObs = BnObsT,
        nTx = nTxT,
        nStudies = nStudiesT
      )
      
      if (!random)
        inits <-
        function() {
          list(beta = c(NA, rnorm(nTx - 1, 0, 2)),
               alpha = rnorm(nStudies))
        }
      if (random)
        inits <-
        function() {
          list(
            beta = c(NA, rnorm(nTx - 1, 0, 2)),
            sd = 0.1,
            alpha = rnorm(nStudies)
          )
        }
      
      subData <<- subData
      subDataMed <<- subDataMed
      subDataBin <<- subDataBin
      
      inits <<- inits
    }
    
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


