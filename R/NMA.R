
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
#' @param fileSep File separator; string
#' 
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
                RUN = TRUE,
                fileSep = "\\") {
  
  browser()
  params_to_save <- c(effectParam, modelParams)
  params_to_save <- params_to_save[!is.na(params_to_save)]
  
  if (RUN) {
    
    res_bugs <-
      # bugs_fn(
      R2OpenBUGS::bugs(
        data = dat$bugsData,
        parameters.to.save = params_to_save,
        model.file = bugs_filename,
        n.chains = bugs_params$N.CHAINS,
        inits = dat$inits(),
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
  
  ## if (DIAGNOSTICS) 
  ##    diagnostic_plot()
  
  ## file_manip()
  
  ## bugs_stats()
  
  ## plots_and_tables()
  
  return(res_bugs)
}

