
#'
plotNetwork <- function(subData,
                        subDataBin,
                        binData,
                        subDataMed,
                        medData,
                        usecurve = FALSE,
                        ...) {
  
  if (!binData & !medData) {
    subDataComb <- subData[, c("study", "tx", "base", "Ltx", "Lbase")]
  }
  
  if (binData & !medData) {
    subDataBinN <- subDataBin
    names(subDataBinN)[c(6, 7)] <- c("Ltx", "Lbase")
    subDataComb <-
      rbind(subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataBinN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (!binData & medData) {
    subDataMedN <- subDataMed
    names(subDataMedN)[c(5, 6)] <- c("Ltx", "Lbase")
    subDataComb <-
      rbind(subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataMedN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (binData & medData) {
    
    subDataBinN <- subDataBin
    subDataMedN <- subDataMed
    
    names(subDataBinN)[names(subDataBinN) == "BinR"] <- "Ltx"
    names(subDataBinN)[names(subDataBinN) == "BinN"] <- "Lbase"
    names(subDataMedN)[names(subDataMedN) == "medR"] <- "Ltx"
    names(subDataMedN)[names(subDataMedN) == "medN"] <- "Lbase"
    
    keep_cols <- c("study", "tx", "base", "Ltx", "Lbase")
    
    subDataComb <-
      rbind(subData[, keep_cols],
            subDataBinN[, keep_cols],
            subDataMedN[, keep_cols])
  }
  
  subDataCombLng <-
    reshape(
      subDataComb,
      direction = "long",
      varying = list(c("Ltx", "Lbase")),
      timevar = "txCode")
  
  subDataCombLng <-
    subDataCombLng[order(subDataCombLng$study), ]
  
  ##TODO: pass as argument  
  nTx <- length(txList)
  
  networkData <- matrix(NA, nTx, nTx,
                        dimnames = list(txList, txList))
  
  for (tx1 in seq_len(nTx)) {
    for (tx2 in tx1:nTx) {
      
      is_tx_from <-
        subDataCombLng$base == txList[tx2] &
        subDataCombLng$tx == txList[tx1]

      is_tx_to <-
        subDataCombLng$base == txList[tx1] &
        subDataCombLng$tx == txList[tx2]
      
      networkData[tx1, tx2] <-
        sum(subDataCombLng[is_tx_from | is_tx_to, "txCode"])
    }
  }
  
  layout(1)
  par(mar = c(2, 2, 2, 2))
  gplot(
    networkData / networkData,
    label = txList,
    edge.lwd = networkData / 200,
    gmode = "graph",
    label.lty = 1)#,
    # ...)
}

