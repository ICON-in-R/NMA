
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
#' @param nma_datasets survDataHR: Hazard ratio data. Optional;
#'                     survDataBin: Survival binary data. Optional;
#'                     survDataMed: Median time data. Optional;
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
        Lstudy = input$survDataHR$dat$Lstudy,
        Ltx = input$survDataHR$dat$Ltx,
        Lbase = input$survDataHR$dat$Lbase,
        Lmean = input$survDataHR$dat$Lmean,
        Lse = input$survDataHR$dat$Lse,
        multi = input$survDataHR$dat$multi_arm,
        LnObs = input$survDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      survDataHR = input$survDataHR$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (use_data$surv_bin_data & !use_data$med_data) {
    
    bugsData <-
      list(
        Lstudy = input$survDataHR$dat$Lstudy,
        Ltx = input$survDataHR$dat$Ltx,
        Lbase = input$survDataHR$dat$Lbase,
        Lmean = input$survDataHR$dat$Lmean,
        Lse = input$survDataHR$dat$Lse,
        multi = input$survDataHR$dat$multi,
        LnObs = input$survDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        Bstudy = input$survDataBin$dat$Bstudy,
        Btx = input$survDataBin$dat$Btx,
        Bbase = input$survDataBin$dat$Bbase,
        Bn = input$survDataBin$dat$BinN,
        Br = input$survDataBin$dat$BinR,
        BnObs = input$survDataBin$BnObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      survDataHR = input$survDataHR$dat,
      survDataBin = input$survDataBin$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (!use_data$surv_bin_data & use_data$med_data) {
    
    bugsData <-
      list(
        Lstudy = input$survDataHR$dat$Lstudy,
        Ltx = input$survDataHR$dat$Ltx,
        Lbase = input$survDataHR$dat$Lbase,
        Lmean = input$survDataHR$dat$Lmean,
        Lse = input$survDataHR$dat$Lse,
        multi = input$survDataHR$dat$multi_arm,
        LnObs = input$survDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        medianStudy = input$survDataMed$dat$medianstudy,
        medianTx = input$survDataMed$dat$mediantx ,
        medianBase = input$survDataMed$dat$medianbase,
        medianN = input$survDataMed$dat$medN,
        medianR = input$survDataMed$dat$medR,
        median = input$survDataMed$dat$median,
        medianNObs = input$survDataMed$medianNObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      survDataHR = input$survDataHR$dat,
      survDataMed = input$survDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
  
  if (use_data$surv_bin_data & use_data$med_data) {
    
    bugsData <-
      list(
        Lstudy = input$survDataHR$dat$Lstudy,
        Ltx = input$survDataHR$dat$Ltx,
        Lbase = input$survDataHR$dat$Lbase,
        Lmean = input$survDataHR$dat$Lmean,
        Lse = input$survDataHR$dat$Lse,
        multi = input$survDataHR$dat$multi_arm,
        LnObs = input$survDataHR$LnObs,
        nTx = input$nTx,
        nStudies = input$nStudies,
        medianStudy = input$survDataMed$dat$medianstudy,
        medianTx = input$survDataMed$dat$mediantx ,
        medianBase = input$survDataMed$dat$medianbase,
        Bstudy = input$survDataBin$dat$Bstudy,
        Btx = input$survDataBin$dat$Btx,
        Bbase = input$survDataBin$dat$Bbase,
        medianN = input$survDataMed$dat$medN,
        medianR = input$survDataMed$dat$medR,
        median = input$survDataMed$dat$median,
        medianNObs = input$survDataMed$medianNObs,
        Bn = input$survDataBin$dat$BinN,
        Br = input$survDataBin$dat$BinR,
        BnObs = input$survDataBin$BnObs)
    
    return(list(
      inits = rinits(input$nTx, input$nStudies, param_names),
      survDataHR = input$survDataHR$dat,
      survDataBin = input$survDataBin$dat,
      survDataMed = input$survDataMed$dat,
      bugsData = bugsData,
      txList = input$txList))
  }
}

