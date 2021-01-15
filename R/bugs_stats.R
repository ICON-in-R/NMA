
#'
bugs_stats <- function() {
  
  dummyOR <- c(1, 1, NA, 1, 1, NA)
  dummy <- c(0, 0, NA, 0, 0, NA)
  
  colEff <- c("Mean", "Median", "SE", "L95CrI", "U95CrI", "Rhat")
  n_effectParam <- length(effectParam)
  
  effectParamName <-
    c("Log Hazard Ratio", "Parameters", "DIC", "Residual Deviance")
  
  EffectRes_lhr <-
    resultsFileSetUp(res_bugs$summary, "beta", dummy, dummyOR, colEff)
  
  para <-
    round(res_bugs$summary[-grep(paste0("^", effectParam[1]),
                                 rownames(res_bugs$summary)), c(1, 5, 2, 3, 7, 8)], 2)
  
  if (n_effectParam > 1) {
    for (ee in 2:length(effectParam)) {
      para <-
        para[-grep(paste0("^", effectParam[ee]), rownames(para)), ]
      
    }
  }
  Deviance <-
    round(res_bugs$summary[grep(paste0("^", "deviance"),
                                rownames(res_bugs$summary)), c(1, 5, 2, 3, 7, 8)], 2)
  Dev <- Deviance
  
  if (random) {
    SD <-
      round(res_bugs$summary[grep(paste0("^", "sd"),
                                  rownames(res_bugs$summary)), c(1, 5, 2, 3, 7, 8)], 2)
  }
  
  para <-  
    if (random) {
      rbind(Deviance, SD)
    } else {
      rbind(Deviance)
    }
  
  colnames(para) <- paste(colEff)
  
  resDev <- res_bugs$summary["totresdev", "mean"]
  DIC <- res_bugs$DIC
  
  results <-
    list(EffectRes_lhr,
         para,
         DIC = DIC,
         resDev = resDev)
}

