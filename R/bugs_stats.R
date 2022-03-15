
#' BUGS statistics
#'
#' @param dat input data; list
#' @param res_bugs output from running BUGS
#' @param effectParam text vector of names
#' @param random RE or FE
#'
#' @return
#' @export
#'
bugs_stats <- function(dat,
                       res_bugs,
                       effectParam,
                       random) {
  
  colEff <- c("Mean", "Median", "SE", "L95CrI", "U95CrI", "Rhat")
  n_effectParam <- length(effectParam)
  
  effectParamName <-
    c("Log Hazard Ratio", "Parameters", "DIC", "Residual Deviance")
  
  effectRes_summary <-
    my_bugs_summary(res_bugs$summary, "beta", dat$txList)
  
  col_idx <- c(1, 5, 2, 3, 7, 8)
  
  para <-
    round(digits = 2,
          res_bugs$summary[-grep(paste0("^", effectParam[1]),
                                 rownames(res_bugs$summary)), col_idx])
  
  if (n_effectParam > 1) {
    for (ee in 2:length(effectParam)) {
      para <-
        para[-grep(paste0("^", effectParam[ee]), rownames(para)), ]
      
    }
  }
  
  Deviance <-
    round(digits = 2,
      res_bugs$summary[grep(paste0("^", "deviance"),
                            rownames(res_bugs$summary)), col_idx])
  if (random) {
    SD <-
      round(digits = 2,
        res_bugs$summary[grep(paste0("^", "sd"),
                              rownames(res_bugs$summary)), col_idx])
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
  
  list(effectRes_summary,
       para,
       DIC = DIC,
       resDev = resDev)
}

