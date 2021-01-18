
#' pairwiseTable
#'
#' @param sims 
#'
#' @return
#' @export
#'
pairwiseTable <- function(sims) {
  
  txList <- colnames(sims)
  nTx <- length(txList)
  
  pairwiseTable <- matrix(NA, nTx, nTx)
  
  for (tx1 in seq_len(nTx)) {
    for (tx2 in seq_len(nTx)) {
      pairwiseTable[tx1, tx2] <-
        paste(
          round(exp(median(
            sims[, tx2] - sims[, tx1], na.rm = TRUE)), 2),
          " (",
          paste(round(exp(
            quantile(sims[, tx2] - sims[, tx1], c(0.025, 0.975), na.rm = TRUE)
          ), 2), collapse = ",", sep = ""),
          ")",
          collapse = "",
          sep = ""
        )
      
    }
  }
  
  colnames(pairwiseTable) <- txList
  rownames(pairwiseTable) <- txList
  
  return(t(pairwiseTable))
}

