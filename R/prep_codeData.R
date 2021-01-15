
#' prep_codeData
#' 
#' Create input data for NMA.
#' 
#' @param subData 
#' @param subDataBin 
#' @param subDataMed 
#' @param refTx 
#' @importFrom purrr map
#' @importFrom dplyr
#' 
#' @return List of data and statistics.
#'         If no binary of median data empty sub-lists.
#' @export
#'
prep_codeData <- function(subData,
                          subDataBin = NA,
                          subDataMed = NA,
                          refTx = NA) {
  
  is_med <- all(!is.na(subDataMed))
  is_bin <- all(!is.na(subDataBin))
  
  dat_list <- list()
  bin_list <- list()
  med_list <- list()
  
  dat <- list(subData,
              subDataMed,
              subDataBin)
  
  # all tx in datasets
  tx_names <-
    sort(unlist(
      map(dat, ~select(., tx, base))))
  
  tx_names <- unique(c(refTx, tx_names))
  
  tx_names <- tx_names[!is.na(tx_names)]
  
  nTx <- length(tx_names)
  
  refTx <- tx_names[1]
  
  study_names <- unique(unlist(map(dat, "study")))
  
  dat_list$dat <-
    subData %>%
    mutate(Ltx = match(tx, tx_names),
           Lbase = match(base, tx_names),
           Lstudy = match(study, study_names)) %>%
    arrange(study, tx) %>% 
    as.data.frame()
  
  dat_list$LnObs <- nrow(subData)
  
  nStudies <- max(dat_list$subData$Lstudy, 2)
  
  if (is_bin) {
    
    bin_list$dat <- 
      subDataBin %>% 
      mutate(Btx = match(tx, tx_names),
             Bbase = match(base, tx_names),
             Bstudy = match(study, study_names)) %>% 
      arrange(study, tx) %>% 
      as.data.frame()
    
    bin_list$BnObs <- nrow(subDataBin)
  }
  
  if (is_med) {
    
    med_list$dat <-
      subDataMed %>% 
      mutate(mediantx = match(tx, tx_names),
             medianbase = match(base, tx_names),
             medianstudy = match(study, study_names)) %>% 
      arrange(study, tx) %>% 
      as.data.frame()
    
    med_list$medianNObs <- nrow(subDataMed)
    
    nStudies <- max(c(nStudies, med_list$subDataMed$medianstudy))
  }
  
  return(list(
    subData = dat_list,
    subDataMed = med_list,
    subDataBin = bin_list,
    nStudies = nStudies,
    nTx = nTx,
    refTx = refTx,
    txList = tx_names))
}

