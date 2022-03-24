
#' Run network meta-analysis
#' 
#' @param nma Object of class \code{nma}
#' @param output_dir Output folder name; string
#' @param save Save BUGS output to file? Logical
#'
#' @return \code{res_bugs}
#' @export
#' @name NMA_run
#' @seealso \code{\link{new_NMA}}, \code{\link{NMA_update}}
#' 
NMA_run <- function(nma,
                    output_dir = "output",
                    save = TRUE) {
  UseMethod("NMA_run", nma)
}


#' @rdname NMA_run
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom here here
#' 
#' @return
#' @export
#' 
NMA_run.nma <- function(nma,
                        output_dir = "output",
                        save = TRUE) {
  dat <- nma$dat
  bugs_params <- nma$bugs_params
  run_bugs <- bugs_params$run_bugs
  
  params_to_save <- c(nma$effectParam, nma$modelParams)
  params_to_save <- params_to_save[!is.na(params_to_save)]
  
  bugs_filename <- make_bugs_filename(nma$is_random, dat)
  labels <- make_labels(nma$label)
  
  ## run bugs model
  
  if (run_bugs) {
    init_vals <- map(1:bugs_params$N.CHAINS,
                     ~dat$inits())
    
    cat("====== RUNNING BUGS MODEL\n")
    
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
    ##TODO: what goes here?
  }
  
  if (save) {
    createFolders(folder = output_dir, 
                  "results", "model", "sims", "data")
    save_bugs_files(res_bugs, bugs_params, run_bugs, labels, output_dir)
    
    file_name <- paste0("bugsObject_", labels$short, ".RData")
    save(res_bugs, file = here::here(file.path(output_dir, "model", file_name)))
  }
  
  res_bugs
}

