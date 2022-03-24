
#' Pairwise table
#'
#' @param nma
#' @param res_bugs
#' @param folder
#' @param probs
#' @param label
#' @param save Logical
#' @param ... Additional arguments
#'
#' @return table
#' @export
#'
pairwiseTable <- function(nma,
                          res_bugs,
                          folder = "output",
                          probs = c(0.025, 0.975),
                          label = "",
                          save = FALSE,
                          ...) {
  dat <- nma$dat
  
  beta_cols <- grep(paste0("^beta"), rownames(res_bugs$summary))
  sims <- res_bugs$sims.matrix[, beta_cols]
  sims <- cbind(0, sims)
  colnames(sims) <- dat$txList
  
  txList <- colnames(sims)
  nTx <- length(txList)
  
  tab <- matrix(NA, nrow = nTx, ncol = nTx, dimnames = list(txList, txList))
  
  for (tx1 in seq_len(nTx)) {
    for (tx2 in seq_len(nTx)) {
      
      medians <- 
        round(exp(median(
          sims[, tx2] - sims[, tx1], na.rm = TRUE)), 2)
      
      iqr <-
        paste(round(exp(
          quantile(sims[, tx2] - sims[, tx1], probs, na.rm = TRUE)
        ), 2), collapse = ",", sep = "")
      
      tab[tx1, tx2] <-
        paste0(medians, " (", iqr,")", collapse = "")
    }
  }
  
  if (save) {
    file_name <- paste0("Pairwise_results_", label, ".csv")
    dir_name <- file.path(folder, "results", file_name)
    
    writeLines(
      paste("Pairwise Treatment Coefficients;",
            "Median hazard ratio (95% Credible Interval)",
            "Row treatment vs. Column treatment (reference)",
            "------------------"),
      dir_name)
    
    suppressWarnings(
      write.table(
        tab,
        file = dir_name,
        sep = ",",
        append = TRUE,
        col.names = NA))
  }
  
  return(t(tab))
}

