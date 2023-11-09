
#' Prepare data used in NMA computation
#' 
#' Create input data for NMA.
#' Renames the study, base and other fields with appended letter
#' depending on type of data.
#' 
#' @param survDataHR Hazard ratio data frame
#' @param survDataBin Optional data frame. Survival binary data
#' @param survDataMed Optional data frame. Median times
#' @param refTx Reference treatment name; string
#' @importFrom purrr map map_if
#' @import dplyr
#' 
#' @return List of data and statistics.
#'         If no binary of median data empty sub-lists.
#' @export
#'
prep_codeData <- function(survDataHR = NA,
                          survDataBin = NA,
                          survDataMed = NA,
                          binData = NA,
                          countData = NA,
                          contsData = NA,
                          refTx = NA) {

  if (all(is.na(survDataHR))) return()
  
  is_med <- all(!is.na(survDataMed))
  is_bin <- all(!is.na(survDataBin))
  
  dat_list <- list()
  bin_list <- list()
  med_list <- list()
  
  dat <- list(survDataHR,
              survDataMed,
              survDataBin)
  
  # all treatments in data sets
  tx_names <-
    sort(unlist(
      map_if(dat,
             .p = ~all(!is.na(.)),         # data provided
             .f = ~select(., tx, base))))  # treatment names
  
  tx_names <- unique(c(refTx, tx_names))
  
  tx_names <- tx_names[!is.na(tx_names)]
  
  nTx <- length(tx_names)
  
  refTx <- tx_names[1]
  
  study_names <- unique(unlist(map(dat, "study")))
  
  dat_list$dat <-
    survDataHR %>%
    mutate(Ltx = match(tx, tx_names),
           Lbase = match(base, tx_names),
           Lstudy = match(study, study_names)) %>%
    arrange(study, tx) %>% 
    as.data.frame()
  
  dat_list$LnObs <- nrow(survDataHR)
  
  nStudies <- max(dat_list$dat$Lstudy, 2)
  
  if (is_bin) {
    
    bin_list$dat <- 
      survDataBin %>% 
      mutate(Btx = match(tx, tx_names),
             Bbase = match(base, tx_names),
             Bstudy = match(study, study_names)) %>% 
      arrange(study, tx) %>% 
      as.data.frame()
    
    bin_list$BnObs <- nrow(survDataBin)
    nStudies <- max(c(nStudies, bin_list$dat$Bstudy))
  }
  
  if (is_med) {
    
    med_list$dat <-
      survDataMed %>% 
      mutate(mediantx = match(tx, tx_names),
             medianbase = match(base, tx_names),
             medianstudy = match(study, study_names)) %>% 
      arrange(study, tx) %>% 
      as.data.frame()
    
    med_list$medianNObs <- nrow(survDataMed)
    nStudies <- max(c(nStudies, med_list$dat$medianstudy))
  }
  
  return(list(
    survDataHR = dat_list,
    survDataMed = med_list,
    survDataBin = bin_list,
    nStudies = nStudies,
    nTx = nTx,
    refTx = refTx,
    txList = tx_names))
}

