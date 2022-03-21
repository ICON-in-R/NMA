
#' Rank probability plot
#' 
#' Treatment Ranking Plot.
#'
#' @param res_bugs
#' @param labels Labels
#' @param dat
#' @param ...
#' 
#' @importFrom gplots balloonplot
#' @return
#' @export
#'
rankProbPlot <- function(res_bugs,
                         labels,
                         dat,
                         save = FALSE,
                         ...) {
  
  beta_cols <- grep(paste0("^beta"), rownames(res_bugs$summary))
  sims <- res_bugs$sims.matrix[, beta_cols]
  sims <- cbind(0, sims)
  colnames(sims) <- dat$txList
  
  dir_name <-
    paste0(folder, fileSep, "graphs", fileSep, "ranking_", labels$short, ".pdf")

  if (save) {
    pdf(file = dir_name)
    on.exit(dev.off(), add = TRUE)
  }
  
  nRanks <- ncol(sims)
  
  sims <- sims[, order(apply(sims, 2, mean), decreasing = TRUE)]
  rankSummary <- numeric()
  ranks <- apply(sims, 1, rank, ties.method = "random")
  
  for (i in seq_len(ncol(sims))) {
    rankSummary <- c(rankSummary, apply(ranks == i, 1, mean))
  }
  
  Ranking <- rep(1:ncol(sims), rep(ncol(sims), ncol(sims)))
  txList <- colnames(sims)
  Treatment <- rep(txList, ncol(sims))
  rankSummary <- as.numeric(round(rankSummary, 2))
  
  txtSize <- 
    if (length(txList) > 12) { 0.35
    } else{ 0.55 }
  
  main.txt <-
    paste(
      labels$orig,
      "Rank based on treatment effect",
      "Rank 1: Most effective treatment (Smallest hazard ratio)",
      sep = "\n")
  
  par(mar = c(2, 2, 3, 2))
  
  gplots::balloonplot(
    x = Ranking,
    y = Treatment,
    z = rankSummary,
    rowmar = 4,
    colmar = 1,
    label.lines = FALSE,
    sort = FALSE,
    show.zeros = TRUE,
    show.margins = FALSE,
    cum.margins = FALSE,
    main = mtext(text = main.txt, cex = 0.6),
    text.size = txtSize,
    label.size = txtSize)
}

