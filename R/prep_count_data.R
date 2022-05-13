
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
  
  ##TODO: what is this doing? remove SQL
  idBase <- function(idVar, byVar) {
    idData <- data.frame(idVar = idVar, byVar = byVar) 
    baseVar <-
      sqldf::sqldf(
        "select baseVar from idData as a inner join (select byVar, min(idVar)
          as baseVar from idData group by byVar) as b on a.byVar=b.byVar")
    unlist(baseVar)
  }
  
  txList <- unique(sort(count_dat$treatment))
  
  count_dat$prop <- paste(count_dat$r,"\\",count_dat$E, sep = " ")   
  
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
  
  count_dat <- count_dat[order(count_dat$numTx, count_dat$study, count_dat$tx), ]
  
  # identify baseline treatment
  ##TODO: remove function and SQL
  count_dat$baseTx <- idBase(idVar = count_dat$tx,
                            byVar = count_dat$study)
  
  # code studies
  studyList <- unique(count_dat$study)
  count_dat$studyCode <- match(count_dat$study, table = studyList)
  nStudies <- max(count_dat$studyCode, 2)
  nObs <- length(count_dat$studyCode)
  
  ##TODO: what is this?...
  if (is.na(preRefTx)) {
    preRefTxCode <- 1
  } else {
    preRefTxCode <- which(txList == preRefTx)
  }
  
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
    tAn[i, 1:tAna[i]] <- count_dat$n[count_dat$studyCode == i]
    tAE[i, 1:tAna[i]] <- count_dat$E[count_dat$studyCode == i]
    tAt[i, 1:tAna[i]] <- count_dat$tx[count_dat$studyCode == i]
  }
  
  list(tAt = tAt,
       tAr = tAr,
       tAn = tAn,
       tAE = tAE,
       nTx = nTx,
       tAna = tAna,
       nStudies = nStudies)  
}
