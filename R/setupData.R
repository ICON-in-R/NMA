
#'
rinits <-
  function(nTx, param_names) {
    function() {
      list(
        beta = c(NA, rnorm(nTx - 1, 0, 2)),
        sd = 0.1,
        alpha = rnorm(nStudies)) %>% 
        .[param_names]
    }
  }


#' TODO: refactor codeData() fns and combine
#' 
setupData <- function(subData,
                      subDataBin,
                      binData,
                      subDataMed,
                      medData,
                      random,
                      refTx) {
  
  param_names <-
    if (random) {
      c("beta", "sd", "alpha")
    } else {
      c("beta", "alpha")}
  
  if (!binData & !medData) {
    
    subData <- codeData(subData, refTx)
    
    bugsData <-
      list(
        Lstudy = subData$Lstudy,
        Ltx = subData$Ltx,
        Lbase = subData$Lbase,
        Lmean = subData$Lmean,
        Lse = subData$Lse,
        multi = subData$multi,
        LnObs = nrow(subData),
        nTx = length(txList),
        nStudies = max(subData$Lstudy))
    
    return(inits = rinits(nTx, param_names),
           subData = subData,
           bugsData = bugsData)
  }
  
  if (binData & medData) {
    
    subDatalist <-
      codeDataBin(subData = subData,
                  subDataBin = subDataBin,
                  refTx = refTx)
    
    subData <- as.data.frame(subDatalist[1])
    subDataBin <- as.data.frame(subDatalist[2])
    
    bugsData <-
      list(
        Lstudy = subData$Lstudy,
        Ltx = subData$Ltx,
        Lbase = subData$Lbase,
        Lmean = subData$Lmean,
        Lse = subData$Lse,
        multi = subData$multi,
        LnObs = nrow(subData),
        nTx = length(txList),
        nStudies = max(subData$Lstudy,
                       subDataBin$Bstudy),
        Bstudy = BstudyT,
        Btx = subDataBin$Btx,
        Bbase = subDataBin$Bbase,
        Bn = subDataBin$BinN,
        Br = subDataBin$BinR,
        BnObs = nrow(subDataBin))
    
    return(inits = rinits(nTx, param_names),
           subData = subData,
           subDataBin = subDataBin,
           bugsData = bugsData)
  }
  
  if (!binData & medData) {
    subDatalist <-
      codeDataMed(subData = subData,
                  subDataMed = subDataMed,
                  refTx = refTx)
    
    subData <- as.data.frame(subDatalist[1])
    subDataMed <- as.data.frame(subDatalist[2])
    
    bugsData <-
      list(
        Lstudy = subData$Lstudy,
        Ltx = subData$Ltx,
        Lbase = subData$Lbase,
        Lmean = subData$Lmean,
        Lse = subData$Lse,
        multi = subData$multi,
        LnObs = nrow(subData),
        nTx = length(txList),
        nStudies = max(subData$Lstudy,
                       subDataMed$mediantudy),
        medianStudy = subDataMed$medianstudy,
        medianTx = subDataMed$mediantx ,
        medianBase = subDataMed$medianbase,
        medianN = subDataMed$medN,
        medianR = subDataMed$medR,
        medianNObs = nrow(subDataMed),
        median = subDataMed$median)
    
    return(inits = rinits(nTx, param_names),
           subData = subData,
           subDataMed = subDataMed,
           bugsData = bugsData)
  }
  
  if (binData & medData) {
    subDatalist <-
      codeDataBinMed(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin,
        refTx = refTx)
    
    subData <- as.data.frame(subDatalist[1])
    subDataMed <- as.data.frame(subDatalist[2])
    subDataBin <- as.data.frame(subDatalist[3])
    
    bugsData <-
      list(
        Lstudy = subData$Lstudy,
        Ltx = subData$Ltx,
        Lbase = subData$Lbase,
        Lmean = subData$Lmean,
        Lse = subData$Lse,
        multi = subData$multi,
        nTx = length(txList),
        nStudies = max(subData$Lstudy,
                       subDataMed$medianstudy,
                       subDataBin$Bstudy),
        medianStudy = subDataMed$medianstudy,
        medianTx = subDataMed$mediantx ,
        medianBase = subDataMed$medianbase,
        Bstudy = subDataBin$Bstudy,
        Btx = subDataBin$Btx,
        Bbase = subDataBin$Bbase,
        LnObs = nrow(subData),
        medianN = subDataMed$medN,
        medianR = subDataMed$medR,
        medianNObs = nrow(subDataMed),
        median = subDataMed$median,
        Bn = subDataBin$BinN,
        Br = subDataBin$BinR,
        BnObs = nrow(subDataBin))
    
    return(inits = rinits(nTx, param_names),
           subData = subData,
           subDataBin = subDataBin,
           subDataMed = subDataMed,
           bugsData = bugsData)
  }
  
}

