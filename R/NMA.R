
#' Network meta-analysis
#' 
#' @param dat result of setupData()
#' @param bugs_params
#' @param bugs_fn BUGS function call
#' @param effectParam 
#' @param modelParams 
#' @param output_dir
#' @param label
#' @param endpoint 
#' @param preRefTx what is this?
#' @param random 
#' @param run_bugs 
#' @param DIAGNOSTICS Produce diagnostic plots; logical
#' @param fileSep File separator; string
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom here here
#' 
#' @return
#' @export
#' 
NMA <- function(dat,
                bugs_params,
                bugs_fn,
                effectParam = NA,
                modelParams = NA,
                output_dir = "output",
                label,
                endpoint = NULL,
                preRefTx = NA,
                random = FALSE,
                run_bugs = TRUE,
                DIAGNOSTICS = TRUE,
                fileSep = "/") {
  
  params_to_save <- c(effectParam, modelParams)
  params_to_save <- params_to_save[!is.na(params_to_save)]
  
  bugs_filename <- make_bugs_filename(random, dat)
  labels <- make_labels(label)
  
  ## run bugs model ----
  
  if (run_bugs) {
    init_vals <- map(1:bugs_params$N.CHAINS, ~dat$inits())
    
    res_bugs <-
      bugs_fn(
      # R2OpenBUGS::bugs(
      # R2jags::jags(
        data = dat$bugsData,
        parameters.to.save = params_to_save,
        model.file = bugs_filename,
        n.chains = bugs_params$N.CHAINS,
        inits = init_vals,
        n.iter =
          (bugs_params$N.SIMS * bugs_params$N.THIN) + bugs_params$N.BURNIN,
        n.burnin = bugs_params$N.BURNIN,
        n.thin = bugs_params$N.THIN
        #codaPkg=FALSE
      )
    
    if (bugs_params$PROG == "JAGS")
      res_bugs <- res_bugs$BUGSoutput
  } else {
    load(file = here(glue("{output_dir}{fileSep}model{fileSep}bugsObject_{labels$short}")))
  }
  
  ## process output ----

  createFolders(folder = output_dir, fileSep,
                "results", "graphs", "model", "sims", "data", "diagnostics")
  save_bugs_files(res_bugs, bugs_params, run_bugs, labels, output_dir, fileSep)
  
  if (DIAGNOSTICS)
     diagnostic_plots(res_bugs, labels)
  
  bugs_stats(dat, res_bugs, effectParam, random)
  
  plots_and_tables(dat, res_bugs, effectParam, labels, endpoint)
  
  save(res_bugs,
       file = glue("{output_dir}{fileSep}model{fileSep}bugsObject_{labels$short}.RData"))
  
  return(res_bugs)
}

 