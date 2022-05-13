
#' prep_bin_data
#'
#' @param binData 
#' @param refTx 
#' @param preRefTx 
#'
#' @return
#' @export
#'
prep_bin_data <- function(binData, refTx = NA, preRefTx = NA) {
  
  txList <- unique(sort(binData$treatment))
  
  binData$prop <- paste(binData$r,"\\",binData$E, sep=" ")   
  
  if(refTx %in% txList && !is.na(refTx)) {
    txList <- unique(c(refTx, sort(binData$treatment)))
  } else {
    refTx <- txList[1]
  }
  
  binData$tx <- match(binData$treatment, table = txList)
  nTx <- length(txList)
  
  for (i in seq_len(nrow(binData))) {
    tx_in_s <- binData[binData$study == binData$study[i], ]$tx
    binData$numTx[i] <- length(unique(tx_in_s))
  }
  
  binData <- binData[order(binData$numTx, binData$study, binData$tx), ]
  
  # base treatment for each study
  tx_lup <- binData |> 
    transmute(base_treatment = treatment,
              baseTx = tx) |> 
    unique()
  
  binData <- 
    binData |> 
    group_by(study) |> 
    mutate(base_treatment = min(treatment)) |> 
    inner_join(tx_lup, by = "base_treatment") 
  
  # code studies
  studyList <- unique(binData$study)
  binData$studyCode <- match(binData$study, table = studyList)
  nStudies <- max(binData$studyCode, 2)
  nObs <- length(binData$studyCode)
  
  long_data <- reshape2::melt(binData, id.vars = c("study","treatment"))
  
  y <- reshape2::dcast(long_data[long_data$variable == "prop", ],
                       study ~ treatment)
  
  wide_data <- NULL
  s_names_rev <- rev(unique(binData$study))
  
  for (i in s_names_rev) {
    wide_data <- rbind(wide_data, y[y$study == i, ])
  }
  
  maxArms <- max(table(binData$study))
  tAna <- rep(NA, nStudies)
  tAr <- matrix(NA, nStudies, maxArms)
  tAt <- matrix(NA, nStudies, maxArms)
  tAn <- matrix(NA, nStudies, maxArms)
  
  for (i in seq_len(nStudies)) {
    tAna[i] <- length(binData$studyCode[binData$studyCode == i])    
    tAr[i, 1:tAna[i]] <- binData$r[binData$studyCode == i]
    tAn[i, 1:tAna[i]] <- binData$n[binData$studyCode == i]
    tAt[i, 1:tAna[i]] <- binData$tx[binData$studyCode == i]
  }
  
  baseProbTx <- 1
  baseData <- binData[binData$tx == baseProbTx, ]
  
  list(baseData = baseData,
       baseProbTx = baseProbTx,
       txList = txList,
       refTx_name = refTx,
       refTx = which(txList == refTx),
       tAt = tAt,
       tAr = tAr,
       tAn = tAn,
       nTx = nTx,
       tAna = tAna,
       nStudies = nStudies)
}
