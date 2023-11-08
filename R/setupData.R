
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
      .[param_names]  # filter redundant
  }
}


#' Set-up study data
#' 
#' Arrange input data for NMA.
#' 
#' @param nma_datasets subDataHR: Hazard ratio data. Optional;
#'                     subDataBin: Survival binary data. Optional;
#'                     subDataMed: Median time data. Optional;
#'                     binData: Binary data. Optional;
#'                     countData: Count data. Optional;
#'                     contsData: Continuous data. Optional
#' @param data_type Data type
#' @param refTx Reference treatment name
#' @param is_random Is this a random effects model? Logical
#' @export
#' @return list
#' 
setupData <- function(nma_datasets,
                      data_type = NA,
                      refTx = NA,
                      is_random = TRUE) {
  
  data_type_names <-
    c("hr_data",
      "surv_bin_data",
      "med_data",
      "bin_data",
      "count_data",
      "conts_data")
  
  use_data <- as.list(data_type_names %in% data_type)
  names(use_data) <- data_type_names
  param_names <- get_param_names(data_type, is_random)
  
  input <- do.call(prep_codeData, c(nma_datasets, refTx = refTx))

  if (use_data$bin_data) {
    
    input <- prep_bin_data(nma_datasets$binData, refTx)  
    
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
           refTx = input$refTx)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      binData = nma_datasets$binData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (use_data$count_data) {
    
    input <- prep_count_data(nma_datasets$countData, refTx)  
    
    bugsData <-
      list(t = input$tAt,
           r = input$tAr,
           E = input$tAE,
           nt = input$nTx,
           na = input$tAna,
           ns = input$nStudies)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      countData = nma_datasets$countData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (use_data$conts_data) {
    
    input <- prep_conts_data(nma_datasets$contsData, refTx)  
    
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
      binData = nma_datasets$contsData,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (!use_data$surv_bin_data & !use_data$med_data) {
    
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
  
  if (use_data$surv_bin_data & !use_data$med_data) {
    
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
  
  if (!use_data$surv_bin_data & use_data$med_data) {
    
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
  
  if (use_data$surv_bin_data & use_data$med_data) {
    
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

