
#' bugs_stats
#'
#' @param res_bugs output from running bugs
#' @param effectParam 
#' @param random RE or FE
#'
#' @return
#' @export
#'
bugs_stats <- function(res_bugs,
                       effectParam,
                       random) {
  
  dummyOR <- c(1, 1, NA, 1, 1, NA)
  dummy <- c(0, 0, NA, 0, 0, NA)
  
  colEff <- c("Mean", "Median", "SE", "L95CrI", "U95CrI", "Rhat")
  n_effectParam <- length(effectParam)
  
  effectParamName <-
    c("Log Hazard Ratio", "Parameters", "DIC", "Residual Deviance")
  
  ##TODO: include extra arguments...
  EffectRes_lhr <-
    my_bugs_summary(res_bugs$summary, "beta") #, dummy, dummyOR, colEff)
  
  col_idx <- c(1, 5, 2, 3, 7, 8)
  
  para <-
    round(res_bugs$summary[-grep(paste0("^", effectParam[1]),
                                 rownames(res_bugs$summary)), col_idx], 2)
  
  if (n_effectParam > 1) {
    for (ee in 2:length(effectParam)) {
      para <-
        para[-grep(paste0("^", effectParam[ee]), rownames(para)), ]
      
    }
  }
  
  Deviance <-
    round(res_bugs$summary[grep(paste0("^", "deviance"),
                                rownames(res_bugs$summary)), col_idx], 2)
  if (random) {
    SD <-
      round(res_bugs$summary[grep(paste0("^", "sd"),
                                  rownames(res_bugs$summary)), col_idx], 2)
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
  
  list(EffectRes_lhr,
       para,
       DIC = DIC,
       resDev = resDev)
}

