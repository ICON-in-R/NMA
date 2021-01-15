
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
#' @param subData 
#' @param refTx 
#' @param subDataBin 
#' @param subDataMed 
#' @param is_random 
#' @export
#' 
setupData <- function(subData,
                      refTx = NA,
                      subDataBin = NA,
                      subDataMed = NA,
                      is_random = TRUE) {
  
  binData <- all(!is.na(subDataBin))
  medData <- all(!is.na(subDataMed))
  
  param_names <-
    if (is_random) {
      c("beta", "sd", "alpha")
    } else {
      c("beta", "alpha")}
  
  dat <-
    prep_codeData(subData,
                  subDataMed,
                  subDataBin,
                  refTx)
  
  if (!binData & !medData) {
    
    bugsData <-
      list(
        Lstudy = dat$Lstudy,
        Ltx = dat$Ltx,
        Lbase = dat$Lbase,
        Lmean = dat$Lmean,
        Lse = dat$Lse,
        multi = dat$multi,
        LnObs = nrow(dat),
        nTx = length(txList),
        nStudies = max(dat$Lstudy))
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = dat$subData,
      bugsData = bugsData))
  }
  
  if (binData & medData) {
    
    bugsData <-
      list(
        Lstudy = dat$Lstudy,
        Ltx = dat$Ltx,
        Lbase = dat$Lbase,
        Lmean = dat$Lmean,
        Lse = dat$Lse,
        multi = dat$multi,
        LnObs = nrow(dat),
        nTx = length(txList),
        nStudies = max(dat$Lstudy,
                       subDataBin$Bstudy),
        Bstudy = dat$Bstudy,
        Btx = subDataBin$Btx,
        Bbase = subDataBin$Bbase,
        Bn = subDataBin$BinN,
        Br = subDataBin$BinR,
        BnObs = nrow(subDataBin))
    
    return(list(
      inits = rinits(nTx, param_names),
      subData = dat$subData,
      subDataBin = dat$subDataBin,
      bugsData = bugsData))
  }
  
  if (!binData & medData) {
    
    bugsData <-
      list(
        Lstudy = dat$Lstudy,
        Ltx = dat$Ltx,
        Lbase = dat$Lbase,
        Lmean = dat$Lmean,
        Lse = dat$Lse,
        multi = dat$multi,
        LnObs = nrow(dat),
        nTx = length(txList),
        nStudies = max(dat$Lstudy,
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
      subData = dat$subData,
      subDataMed = dat$subDataMed,
      bugsData = bugsData))
  }
  
  if (binData & medData) {
    
    bugsData <-
      list(
        Lstudy = dat$Lstudy,
        Ltx = dat$Ltx,
        Lbase = dat$Lbase,
        Lmean = dat$Lmean,
        Lse = dat$Lse,
        multi = dat$multi,
        nTx = length(txList),
        nStudies = max(dat$Lstudy,
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
      subData = dat$subData,
      subDataBin = dat$subDataBin,
      subDataMed = dat$subDataMed,
      bugsData = bugsData))
  }
}

