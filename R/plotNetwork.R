
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
  keep_dat <- names(dat) %in% c("subDataBin", "subDataMed", "binData")  #, "subDataHR"
  study_data <- dat[keep_dat]
  
  for (i in seq_along(study_data)) {
    
    # change to same column names across data types
    names(study_data[[i]])[names(study_data[[i]]) %in% c("BinR", "medR", "r")] <- "Ltx"
    names(study_data[[i]])[names(study_data[[i]]) %in% c("BinN", "medN", "n")] <- "Lbase"
    
    names(study_data[[i]])[names(study_data[[i]]) %in% "treatment"] <- "tx"
  
    # missing column added
    # assume base first treatment
    if (!"base" %in% names(study_data[[i]])) {
      study_data[[i]] <-
        study_data[[i]] |> 
        group_by(study) |> 
        mutate(base = first(tx))
    }
      
    keep_cols <- c("study", "tx", "base", "Ltx", "Lbase")
    study_data[[i]] <- study_data[[i]][, keep_cols]
  }  
  
  # combine to single array
  subDataComb <- do.call(rbind, study_data)
  
  subDataCombLng <-
    melt(subDataComb,
         id.vars = c(1,2,3),
         variable.name = "txCode",
         value.name = "Ltx") |> 
    mutate(txCode = as.numeric(txCode)) |> 
    arrange(study)
  
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

