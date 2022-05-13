
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
  
  ##TODO: what is this doing? remove SQL
  idBase <- function(idVar, byVar) {
    idData <- data.frame(idVar = idVar, byVar = byVar) 
    baseVar <-
      sqldf::sqldf(
        "select baseVar from idData as a inner join (select byVar, min(idVar)
          as baseVar from idData group by byVar) as b on a.byVar=b.byVar")
    unlist(baseVar)
  }
  
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
  
  # identify baseline treatment
  ##TODO: remove function
  binData$baseTx <- idBase(idVar = binData$tx,
                           byVar = binData$study)
  
  # code studies
  studyList <- unique(binData$study)
  binData$studyCode <- match(binData$study, table = studyList)
  nStudies <- max(binData$studyCode, 2)
  nObs <- length(binData$studyCode)
  
  if (is.na(preRefTx)) {
    preRefTxCode <- 1
  } else {
    preRefTxCode <- which(txList == preRefTx)
  }
  
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
  TxList <- unique(sort(binData$treatment))
  
  list(baseData = baseData,
       baseProbTx = baseProbTx,
       preRefTxCode,
       TxList = TxList,
       tAt = tAt,
       tAr = tAr,
       tAn = tAn,
       nTx = nTx,
       tAna = tAna,
       nStudies = nStudies)
}
