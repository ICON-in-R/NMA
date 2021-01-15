
#' Network meta-analysis
#' 
#' @param bugs_filename
#' @param dat result of setupData()
#' @param bugs_params
#' @param bugs_fn BUGS function call
#' @param effectParam 
#' @param modelParams 
#' @param folder why is this PFS?
#' @param label 
#' @param endpoint 
#' @param preRefTx what is this?
#' @param decEff 
#' @param random 
#' @param lg what is this??
#' @importFrom  glue glue
#' 
#' @return
#' @export
#' 
NMA <- function(bugs_filename,
                dat,
                bugs_params,
                bugs_fn,
                effectParam = NA,
                modelParams = NA,
                folder,
                label,
                endpoint,
                preRefTx = NA,
                decEff,
                random = FALSE,
                lg,
                RUN = TRUE) {
  
  all_params <- c(effectParam, modelParams)
  all_params <- all_params[!is.na(all_params)]
  
  if (RUN) {
    
    res_bugs <-
      bugs_fn(
        data = bugsData,
        parameters.to.save = params_to_save,
        model.file = bugs_filename,
        n.chains = bugs_params$N.CHAINS,
        inits = inits,
        n.iter =
          (bugs_params$N.SIMS * bugs_params$N.THIN) + bugs_params$N.BURNIN,
        n.burnin = bugs_params$N.BURNIN,
        n.thin = bugs_params$N.THIN
        #codaPkg=FALSE
      )
    
    if (bugs_params$PROG == "JAGS")
      res_bugs <- res_bugs$BUGSoutput
  } else {
    load(file = glue("{folder}{fileSep}model{fileSep}bugsObject_{slabel}"))
  }
  
  # diagnostic_plot()
  
  ## file_manip()
  
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
  
  
  # plots_and_tables()
  
  # global variables  
  #  resDev
  #  Dev
  # slabel
  # sdlabel
  
  
  
  return(subData)
}

