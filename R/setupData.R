
#' Randomly generate MCMC initial values
#' 
#' @param nTx Number of treatments
#' @param nStudies Number of studies
#' @param param_names Parameter names for specific model and data
#' @return closure
#' 
rinits <- function(nTx, nStudies, param_names) {
  force(nTx)
  force(nStudies)
  force(param_names)
  
  function() {
    list(
      beta = c(NA, rnorm(nTx - 1, 0, 2)),
      sd = 0.1,
      alpha = rnorm(nStudies),
      d = c(NA, rnorm(nTx - 1, 0, 2)),  ##TODO: can we remove duplication?
      mu = rnorm(nStudies),
      baseLod = 0) %>% 
      .[param_names]  # filter
  }
}


#' Set-up study data
#' 
#' Arrange input data for NMA.
#' 
#' @param subDataHR Hazard ratio data. Optional
#' @param subDataBin Survival binary data. Optional
#' @param subDataMed Median time data. Optional
#' @param binData Binary data. Optional
#' @param countData Count data. Optional
#' @param contsData Continuous data. Optional
#' @param data_type Data type
#' @param refTx Reference treatment name
#' @param is_random Is this a random effects model? Logical
#' @export
#' @return list
#' 
setupData <- function(subDataHR = NA,
                      subDataBin = NA,
                      subDataMed = NA,
                      binData = NA,
                      countData = NA,
                      contsData = NA,
                      data_type = NA,
                      refTx = NA,
                      is_random = TRUE) {
  
  is_data <- 
    list(surv_bin = all(!is.na(subDataBin)),
         med = all(!is.na(subDataMed)),
         bin = all(!is.na(binData)),
         count = all(!is.na(countData)),
         conts = all(!is.na(contsData)))
  
  param_names <- do.call(get_param_names, is_data)
  
  input <-
    prep_codeData(subDataHR,
                  subDataBin,
                  subDataMed,
                  refTx)
  
  if (is_data$bin) {
    
    input <- prep_bin_data(binData, refTx)  
    
    bugsData <-
      list(t = input$tAt,
           r = input$tAr,
           n = input$tAn,
           nt = input$nTx,
           na = input$tAna,
           ns = input$nStudies,
           baseR = c(input$baseData$r, NA),
           baseN = c(input$baseData$n, 1),
           nBase = (length(input$baseData$n) + 1),
           baseTx = input$baseProbTx,
           refTx = input$preRefTxCode)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      binData = binData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_data$count) {
    
    input <- prep_count_data(countData, refTx)  
    
    bugsData <-
      list(t = input$tAt,
           r = input$tAr,
           E = input$tAE,
           nt = input$nTx,
           na = input$tAna,
           ns = input$nStudies)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      countData = countData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_data$conts) {
    
    input <- prep_conts_data(contsData, refTx)  
    
    bugsData <-
      list(t = input$tAt,
           y = input$tAy,
           se = input$tAse,
           nt = input$nTx,
           na = input$tAna,
           x = input$tAx,
           ns = input$nStudies,
           mx = input$mx)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      binData = contsData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (!is_data$surv_bin & !is_data$med) {
    
    bugsData <-
      list(
        Lstudy = input$subDataHR$dat$Lstudy,
        Ltx = input$subDataHR$dat$Ltx,
        Lbase = input$subDataHR$dat$Lbase,
        Lmean = input$subDataHR$dat$Lmean,
        Lse = input$subDataHR$dat$Lse,
        multi = input$subDataHR$dat$multi_arm,
        LnObs = input$subDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      subDataHR = input$subDataHR$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_data$surv_bin & !is_data$med) {
    
    bugsData <-
      list(
        Lstudy = input$subDataHR$dat$Lstudy,
        Ltx = input$subDataHR$dat$Ltx,
        Lbase = input$subDataHR$dat$Lbase,
        Lmean = input$subDataHR$dat$Lmean,
        Lse = input$subDataHR$dat$Lse,
        multi = input$subDataHR$dat$multi,
        LnObs = input$subDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        Bstudy = input$subDataBin$dat$Bstudy,
        Btx = input$subDataBin$dat$Btx,
        Bbase = input$subDataBin$dat$Bbase,
        Bn = input$subDataBin$dat$BinN,
        Br = input$subDataBin$dat$BinR,
        BnObs = input$subDataBin$BnObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      subDataHR = input$subDataHR$dat,
      subDataBin = input$subDataBin$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (!is_data$surv_bin & is_data$med) {
    
    bugsData <-
      list(
        Lstudy = input$subDataHR$dat$Lstudy,
        Ltx = input$subDataHR$dat$Ltx,
        Lbase = input$subDataHR$dat$Lbase,
        Lmean = input$subDataHR$dat$Lmean,
        Lse = input$subDataHR$dat$Lse,
        multi = input$subDataHR$dat$multi_arm,
        LnObs = input$subDataHR$LnObs,
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
      inits = rinits(input$nTx, input$nStudies, param_names),
      subDataHR = input$subDataHR$dat,
      subDataMed = input$subDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (is_data$surv_bin & is_data$med) {
    
    bugsData <-
      list(
        Lstudy = input$subDataHR$dat$Lstudy,
        Ltx = input$subDataHR$dat$Ltx,
        Lbase = input$subDataHR$dat$Lbase,
        Lmean = input$subDataHR$dat$Lmean,
        Lse = input$subDataHR$dat$Lse,
        multi = input$subDataHR$dat$multi_arm,
        LnObs = input$subDataHR$LnObs,
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
      subDataHR = input$subDataHR$dat,
      subDataBin = input$subDataBin$dat,
      subDataMed = input$subDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
}

