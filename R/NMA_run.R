
#' @param nma 
#' @param fileSep 
#' @param output_dir 
#' @param save 
#'
#' @return
#' @export
#'
NMA_run <- function(nma,
                    fileSep = "/",
                    output_dir = "output",
                    save = TRUE) {
  UseMethod("NMA_run", nma)
}


#' Network meta-analysis
#' 
#' @param nma object of class nma
#' @param output_dir 
#' @param fileSep File separator; string
#' @param save save bugs output to file; logical
#' 
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom here here
#' 
#' @return
#' @export
#' 
NMA_run.nma <- function(nma,
                        fileSep = "/",
                        output_dir = "output",
                        save = TRUE) {
  dat <- nma$dat
  bugs_params <- nma$bugs_params
  run_bugs <- bugs_params$run_bugs
  
  params_to_save <- c(nma$effectParam, nma$modelParams)
  params_to_save <- params_to_save[!is.na(params_to_save)]
  
  bugs_filename <- make_bugs_filename(nma$is_random, dat)
  labels <- make_labels(nma$label)
  
  ## run bugs model ----
  
  if (run_bugs) {
    init_vals <- map(1:bugs_params$N.CHAINS,
                     ~dat$inits())
    
    res_bugs <-
      nma$bugs_fn(
        data = dat$bugsData,
        parameters.to.save = params_to_save,
        model.file = bugs_filename,
        n.chains = bugs_params$N.CHAINS,
        inits = init_vals,
        n.iter =
          (bugs_params$N.SIMS * bugs_params$N.THIN) + bugs_params$N.BURNIN,
        n.burnin = bugs_params$N.BURNIN,
        n.thin = bugs_params$N.THIN)
    
    if (bugs_params$PROG == "JAGS")
      res_bugs <- res_bugs$BUGSoutput
  } else {
    load(here(glue("{output_dir}{fileSep}model{fileSep}bugsObject_{labels$short}")))
  }

  createFolders(folder = output_dir, fileSep,
                "results", "model", "sims", "data")
  save_bugs_files(res_bugs, bugs_params, run_bugs, labels, output_dir, fileSep)
  
  save(res_bugs,
       file = here(glue("{output_dir}{fileSep}model{fileSep}bugsObject_{labels$short}.RData")))
  
  return(res_bugs)
}

