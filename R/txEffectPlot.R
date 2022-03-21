
#' Treatment effect plot
#'
#' @param dat Study input data; list
#' @param res_bugs Simulations
#' @param labels Labels
#' @param endpoint End point name; string
#' @param preRefTx Reference treatment
#' @param ...
#'
#' @return dat
#' @export
#'
txEffectPlot <- function(dat,
                         res_bugs,
                         labels,
                         endpoint = NULL,
                         preRefTx = NA,
                         save = FALSE,
                         ...) {
  
  beta_cols <- grep(paste0("^beta"), rownames(res_bugs$summary))
  sims <- res_bugs$sims.matrix[, beta_cols]
  sims <- cbind(0, sims)
  colnames(sims) <- dat$txList
  
  dir_name <-
    paste0(folder, fileSep, "graphs", fileSep, "forest_", labels$short, ".pdf")

  if (save) {
    pdf(file = dir_name)
    on.exit(dev.off(), add = TRUE)
  }
  
  txListSims <- colnames(sims)
  nTx <- dat$bugsData$nTx
  
  sims <- 
    if (!is.na(preRefTx) &
        preRefTx %in% txListSims) {
      sims - sims[, preRefTx]
    } else {
      sims - sims[, dat$txList[1]]
    }
  
  res <-
    round(digits = 2,
          exp(t(
            apply(sims, 2, summStat))))
  
  res <-
    res[order(res[, 2], decreasing = TRUE), ]
  
  res[res == 0] <- 0.001
  
  txList <- rownames(res)
  nTx <- length(txList)
  
  layout(cbind(1, 2),
         widths = c(2, 1))
  par(mar = c(5, 12, 2, 1),
      cex = 0.8)
  
  plot(
    res[, 2],
    1:nTx,
    ylim = c(0.75, nTx + 0.25),
    xlim = (range(res[, 3], res[, 4])),
    yaxt = "n",
    xaxt = "n",
    main = labels$orig,
    cex.main = 0.6,
    xlab = c(endpoint, "Median hazard ratio (95% CrI)"),
    ylab = " ",
    pch = 19,
    type = "n",
    log = "x",
    cex = 0.8)
  
  axis(1,
       at =    c(round(0.5 ^ seq(5:1), 3), 1, round(1 / (0.5 ^ seq(1:4)))),
       label = c(round(0.5 ^ seq(5:1), 3), 1, round(1 / (0.5 ^ seq(1:4)))),
       cex.axis = 0.8)
  abline(v = 1, col = "grey")
  
  axis(2,
       at = 1:nTx,
       labels = txList,
       las = 2,
       cex.axis = 0.8,
       cex = 0.8)
  
  for (ii in seq_len(nTx)) {
    lines(c(res[ii, 3], res[ii, 4]),
          c(ii, ii),
          col = "black",
          lwd = 2)
  }
  
  for (ii in seq_len(nTx)) {
    points(
      res[ii, 2],
      ii,
      pch = 21,
      cex = 2,
      bg = "black",
      col = "white",
      lwd = 2)
  }
  
  par(mar = c(5, 0, 2, 0))
  plot(
    rep(0, nTx),
    1:nTx,
    xlim = c(0, 1),
    ylim = c(0.75, nTx + 0.25),
    yaxt = "n",
    type = "n",
    bty = "n",
    xaxt = "n",
    ylab = " ",
    xlab = " ")
  
  for (ii in seq_len(nTx)) {
    if (res[ii, 2] == res[ii, 3] &
        res[ii, 2] == res[ii, 3]) {
      text(x = 0, y = ii,
           "Reference Treatment",
           pos = 4,
           cex = 0.8)
    } else {
      text(x = 0, y = ii,
        paste0(res[ii, 2], " (", res[ii, 3],
               " to ", res[ii, 4], ")"),
        pos = 4,
        cex = 0.8)
    }
  }
  
  invisible(dat)
}

