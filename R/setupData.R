
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


#' setupData
#' 
#' TODO: refactor codeData() fns and combine
#' 
#' @param subData 
#' @param refTx 
#' @param subDataBin 
#' @param subDataMed 
#' @param random 
#' @export
#' 
setupData <- function(subData,
                      refTx,
                      subDataBin = NA,
                      subDataMed = NA,
                      random = TRUE) {
  
  binData <- !is.na(subDataBin)
  medData <- !is.na(subDataMed)
  
  param_names <-
    if (random) {
      c("beta", "sd", "alpha")
    } else {
      c("beta", "alpha")}
  
  subDatalist <-
    prep_codeData(subData,
                  subDataMed,
                  subDataBin,
                  refTx)
  
  if (!binData & !medData) {
    
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
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = subDatalist$subData,
      bugsData = bugsData))
  }
  
  if (binData & medData) {
    
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
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = subDatalist$subData,
      subDataBin = subDatalist$subDataBin,
      bugsData = bugsData))
  }
  
  if (!binData & medData) {
    
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
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = subDatalist$subData,
      subDataMed = subDatalist$subDataMed,
      bugsData = bugsData))
  }
  
  if (binData & medData) {
    
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
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = subDatalist$subData,
      subDataBin = subDatalist$subDataBin,
      subDataMed = subDatalist$subDataMed,
      bugsData = bugsData))
  }
}

