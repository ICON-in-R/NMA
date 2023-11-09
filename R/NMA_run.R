
#' Run network meta-analysis
#' 
#' Calling BUGS an NMA is run using the `nma` object created by `new_NMA()`.
#' The BUGS code is taken from DSU reports and papers.
#' 
#' The survival analysis data used is from the \insertCite{Woods2010}{NMA} model.
#' 
#' @template args-nma
#' @param folder Output folder name; string
#' @param save Save BUGS output to file? Logical
#' @importFrom Rdpack reprompt
#' 
#' @return `res_bugs`
#' @export
#' @name NMA_run
#' @seealso `new_NMA()`, `NMA_update()`
#' 
#' @references
#' \insertAllCited{}
#'   
NMA_run <- function(nma,
                    folder = "output",
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
                        folder = "output",
                        save = TRUE) {
  dat <- nma$dat
  bugs_params <- nma$bugs_params
  run_bugs <- bugs_params$run_bugs
  
  params_to_save <- c(nma$effectParam, nma$modelParams)
  params_to_save <- params_to_save[!is.na(params_to_save)]
  
  surv_data_types <- c("hr_data", "surv_bin_data", "med_data") 
  
  if (any(surv_data_types %in% nma$data_type)) {
    bugs_model <- create_bugs_code(random = nma$is_random, dat = nma)
    bugs_filename <- file.path(tempdir(), "bugs_model.txt")
    write(bugs_model, file = bugs_filename)
  } else {
    bugs_filename <- make_bugs_filename(nma$is_random, nma$data_type)
  }
  
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
    new_nma_output_dir(nma_model, folder)
    
    save_bugs_files(res_bugs, bugs_params, run_bugs, labels, folder)
    
    file_name <- paste0("bugsObject_", labels$short, ".RData")
    save(res_bugs, file = here::here(fs::path(folder, "model", file_name)))
  }
  
  res_bugs
}

