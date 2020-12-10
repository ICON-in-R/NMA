
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

