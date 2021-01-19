
#' randomly generate MCMC initial values
#' closure
#' 
rinits <-
  function(nTx, nStudies, param_names) {
    force(nTx)
    force(nStudies)
    force(param_names)
    
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
#' Arrange input data for NMA.
#' 
#' @param subData 
#' @param refTx Reference treatment name
#' @param subDataBin 
#' @param subDataMed 
#' @param is_random Is this a random effects model?
#' @export
#' @return list
#' 
setupData <- function(subData,
                      refTx = NA,
                      subDataBin = NA,
                      subDataMed = NA,
                      is_random = TRUE) {
  
  is_bin <- all(!is.na(subDataBin))
  is_med <- all(!is.na(subDataMed))
  
  param_names <-
    if (is_random) {
      c("beta", "sd", "alpha")
    } else {
      c("beta", "alpha")}
  
  input <-
    prep_codeData(subData,
                  subDataBin,
                  subDataMed,
                  refTx)
  
  if (!is_bin & !is_med) {
    
    bugsData <-
      list(
        Lstudy = input$subData$dat$Lstudy,
        Ltx = input$subData$dat$Ltx,
        Lbase = input$subData$dat$Lbase,
        Lmean = input$subData$dat$Lmean,
        Lse = input$subData$dat$Lse,
        multi = input$subData$dat$multi_arm,
        LnObs = input$subData$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, input$param_names),
      subData = input$subData$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_bin & !is_med) {
    
    bugsData <-
      list(
        Lstudy = input$subData$dat$Lstudy,
        Ltx = input$subData$dat$Ltx,
        Lbase = input$subData$dat$Lbase,
        Lmean = input$subData$dat$Lmean,
        Lse = input$subData$dat$Lse,
        multi = input$subData$dat$multi,
        LnObs = input$subData$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        Bstudy = input$subDataBin$dat$Bstudy,
        Btx = input$subDataBin$dat$Btx,
        Bbase = input$subDataBin$dat$Bbase,
        Bn = input$subDataBin$dat$BinN,
        Br = input$subDataBin$dat$BinR,
        BnObs = input$subDataBin$BnObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, input$param_names),
      subData = input$subData$dat,
      subDataBin = input$subDataBin$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (!is_bin & is_med) {
    
    bugsData <-
      list(
        Lstudy = input$subData$dat$Lstudy,
        Ltx = input$subData$dat$Ltx,
        Lbase = input$subData$dat$Lbase,
        Lmean = input$subData$dat$Lmean,
        Lse = input$subData$dat$Lse,
        multi = input$subData$dat$multi_arm,
        LnObs = input$subData$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        medianStudy = input$subDataMed$dat$medianstudy,
        medianTx = input$subDataMed$dat$mediantx ,
        medianBase = input$subDataMed$dat$medianbase,
        medianN = input$subDataMed$dat$medN,
        medianR = input$subDataMed$dat$medR,
        median = input$subDataMed$dat$median,
        medianNObs = input$subDataMed$medianNObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, input$param_names),
      subData = input$subData$dat,
      subDataMed = input$subDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_bin & is_med) {
    
    bugsData <-
      list(
        Lstudy = input$subData$dat$Lstudy,
        Ltx = input$subData$dat$Ltx,
        Lbase = input$subData$dat$Lbase,
        Lmean = input$subData$dat$Lmean,
        Lse = input$subData$dat$Lse,
        multi = input$subData$dat$multi_arm,
        LnObs = input$subData$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        medianStudy = input$subDataMed$dat$medianstudy,
        medianTx = input$subDataMed$dat$mediantx ,
        medianBase = input$subDataMed$dat$medianbase,
        Bstudy = input$subDataBin$dat$Bstudy,
        Btx = input$subDataBin$dat$Btx,
        Bbase = input$subDataBin$dat$Bbase,
        medianN = input$subDataMed$dat$medN,
        medianR = input$subDataMed$dat$medR,
        median = input$subDataMed$dat$median,
        medianNObs = input$subDataMed$medianNObs,
        Bn = input$subDataBin$dat$BinN,
        Br = input$subDataBin$dat$BinR,
        BnObs = input$subDataBin$BnObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      subData = input$subData$dat,
      subDataBin = input$subDataBin$dat,
      subDataMed = input$subDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
}

