
#' Prep Count Data
#'
#' @param count_dat 
#' @param refTx 
#' @importFrom sqldf sqldf
#' @importFrom reshape2 melt dcast
#' @return
#' @export
#'
prep_count_data <- function(count_dat, refTx, preRefTx = NA) {
  
  txList <- unique(sort(count_dat$treatment))
  
  count_dat$prop <- paste(count_dat$r, "\\", count_dat$E, sep = " ")   
  
  if(refTx %in% txList && !is.na(refTx)) {
    txList <- unique(c(refTx, sort(count_dat$treatment)))
  } else {
    refTx <- txList[1]
  }
  
  count_dat$tx <- match(count_dat$treatment, table = txList)
  nTx <- length(txList)
  
  for (i in seq_len(nrow(count_dat))) {
    tx_in_s <- count_dat[count_dat$study == count_dat$study[i], ]$tx
    count_dat$numTx[i] <- length(unique(tx_in_s))
  }
  
  count_dat <- count_dat |> arrange(numTx, study, tx)
  
  # base treatment for each study
  tx_lup <- count_dat |> 
    transmute(base_treatment = treatment,
              baseTx = tx) |> 
    unique()
  
  count_dat <- 
    count_dat |> 
    group_by(study) |> 
    mutate(base_treatment = min(treatment)) |> 
    inner_join(tx_lup, by = "base_treatment") 
  
  # code studies
  studyList <- unique(count_dat$study)
  count_dat$studyCode <- match(count_dat$study, table = studyList)
  nStudies <- max(count_dat$studyCode, 2)
  nObs <- length(count_dat$studyCode)
  
  long_data <- reshape2::melt(count_dat, id.vars = c("study","treatment"))
  
  y <- reshape2::dcast(long_data[long_data$variable == "prop", ],
                       study ~ treatment)
  
  wide_data <- NULL
  
  s_names_rev <- rev(unique(count_dat$study))
  
  for (i in s_names_rev) {
    wide_data <- rbind(wide_data, y[y$study == i, ])
  }
  
  maxArms <- max(table(count_dat$study))
  tAna <- rep(NA, nStudies)
  tAr <- matrix(NA, nStudies, maxArms)
  tAt <- matrix(NA, nStudies, maxArms)
  tAn <- matrix(NA, nStudies, maxArms)
  tAE <- matrix(NA, nStudies, maxArms)
  
  for (i in seq_len(nStudies)) {
    tAna[i] <- length(count_dat$studyCode[count_dat$studyCode == i])    
    tAr[i, 1:tAna[i]] <- count_dat$r[count_dat$studyCode == i]
    tAE[i, 1:tAna[i]] <- count_dat$E[count_dat$studyCode == i]
    tAt[i, 1:tAna[i]] <- count_dat$tx[count_dat$studyCode == i]
  }
  
  list(tAt = tAt,
       tAr = tAr,
       tAE = tAE,
       nTx = nTx,
       tAna = tAna,
       txList = txList,
       refTx_name = refTx,
       refTx = which(txList == refTx),
       nStudies = nStudies)  
}
