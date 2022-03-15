
#' Plot network diagram
#'
#' @param dat List of study data, including subData
#'  and possibly subDataBin and subDataMed
#' @param usecurve Which line to use? Logical 
#' @param ... Additional arguments
#' @export
#' @name plotNetwork
#' 
plotNetwork <- function(dat,
                        usecurve = FALSE) {
  UseMethod("plotNetwork", dat)
}


#' @rdname plotNetwork
#' @importFrom sna gplot
#' 
#' @return
#' @export
#'
plotNetwork.default <- function(dat,
                                usecurve = FALSE,
                                ...) {
  
  is_bin <- all(!is.na(dat$subDataBin))
  is_med <- all(!is.na(dat$subDataMed))
  
  if (!is_bin & !is_med) {
    subDataComb <- subData[, c("study", "tx", "base", "Ltx", "Lbase")]
  }
  
  if (is_bin & !is_med) {
    subDataBinN <- dat$subDataBin
    names(subDataBinN)[c(6, 7)] <- c("Ltx", "Lbase")
    subDataComb <-
      rbind(subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataBinN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (!is_bin & is_med) {
    subDataMedN <- subDataMed
    names(subDataMedN)[c(5, 6)] <- c("Ltx", "Lbase")
    subDataComb <-
      rbind(subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataMedN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (is_bin & is_med) {
    
    subDataBinN <- dat$subDataBin
    subDataMedN <- dat$subDataMed
    
    names(subDataBinN)[names(subDataBinN) == "BinR"] <- "Ltx"
    names(subDataBinN)[names(subDataBinN) == "BinN"] <- "Lbase"
    names(subDataMedN)[names(subDataMedN) == "medR"] <- "Ltx"
    names(subDataMedN)[names(subDataMedN) == "medN"] <- "Lbase"
    
    keep_cols <- c("study", "tx", "base", "Ltx", "Lbase")
    
    subDataComb <-
      rbind(dat$subData[, keep_cols],
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
  nTx <- length(dat$txList)
  
  networkData <- matrix(NA, nTx, nTx,
                        dimnames = list(dat$txList, dat$txList))
  
  for (tx1 in seq_len(nTx)) {
    for (tx2 in tx1:nTx) {
      
      is_tx_from <-
        subDataCombLng$base == dat$txList[tx2] &
        subDataCombLng$tx == dat$txList[tx1]
      
      is_tx_to <-
        subDataCombLng$base == dat$txList[tx1] &
        subDataCombLng$tx == dat$txList[tx2]
      
      networkData[tx1, tx2] <-
        sum(subDataCombLng[is_tx_from | is_tx_to, "txCode"])
    }
  }
  
  layout(1)
  par(mar = c(2, 2, 2, 2))
  sna::gplot(dat = networkData, #networkData > 0,
             label = dat$txList,
             edge.lwd = networkData / 200,
             gmode = "graph",
             # mode = "circle",
             label.lty = 1)#,
  # ...)
}


#' @rdname plotNetwork
#' @export
#'
plotNetwork.nma <- function(dat,
                            usecurve = FALSE,
                            ...) {
  dat <- dat$dat
  NextMethod() 
}

