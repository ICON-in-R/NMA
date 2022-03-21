
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
  
  is_bin <- !(any(is.na(dat$subDataBin)) || any(is.null(dat$subDataBin)))
  is_med <- !(any(is.na(dat$subDataMed)) || any(is.null(dat$subDataMed)))
  
  if (!is_bin & !is_med) {
    subDataComb <- dat$subData[, c("study", "tx", "base", "Ltx", "Lbase")]
  }
  
  if (is_bin & !is_med) {
    subDataBinN <- dat$subDataBin
    
    names(subDataBinN)[names(subDataBinN) == "BinR"] <- "Ltx"
    names(subDataBinN)[names(subDataBinN) == "BinN"] <- "Lbase"

    subDataComb <-
      rbind(dat$subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataBinN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (!is_bin & is_med) {
    subDataMedN <- dat$subDataMed
    
    names(subDataMedN)[names(subDataMedN) == "medR"] <- "Ltx"
    names(subDataMedN)[names(subDataMedN) == "medN"] <- "Lbase"
    
    subDataComb <-
      rbind(data$subData[, c("study", "tx", "base", "Ltx", "Lbase")],
            subDataMedN[, c("study", "tx", "base", "Ltx", "Lbase")])
  }
  
  if (is_bin & is_med) {
    
    subDataBinN <- dat$subDataBin
    subDataMedN <- dat$subDataMed

    names(subDataBinN)[names(subDataBinN) == "BinR"] <- "Ltx"
    names(subDataMedN)[names(subDataMedN) == "medR"] <- "Ltx"
    names(subDataBinN)[names(subDataBinN) == "BinN"] <- "Lbase"
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

