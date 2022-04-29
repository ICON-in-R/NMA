#' ### old version
#' 
#' #' Perform a network meta-analysis
#' #' 
#' #' @param dat Result of \code{setupData}
#' #' @param bugs_params List of BUGS parameters
#' #' @param effectParam Effect parameters. Optional
#' #' @param output_dir Name of output folder; string
#' #' @param label
#' #' @param endpoint Name of end point; string
#' #' @param preRefTx what is this? Reference treatment
#' #' @param random Random effects model?
#' #' @param run_bugs Logical
#' #' @param DIAGNOSTICS Produce diagnostic plots; logical
#' #' 
#' #' @importFrom glue glue
#' #' @importFrom purrr map
#' #' @importFrom here here
#' #' @seealso \code{\link{NMA_run}}, \code{\link{NMA_update}}
#' #' 
#' #' @return \code{res_bugs}
#' #' @export
#' #' 
#' NMA <- function(dat,
#'                 bugs_params,
#'                 effectParam = NA,
#'                 output_dir = "output",
#'                 label,
#'                 endpoint = NULL,
#'                 preRefTx = NA,
#'                 random = FALSE,
#'                 run_bugs = TRUE,
#'                 DIAGNOSTICS = TRUE) {
#'   
#'   params_to_save <- c(effectParam, modelParams)
#'   params_to_save <- params_to_save[!is.na(params_to_save)]
#'   
#'   bugs_filename <- make_bugs_filename(random, dat)
#'   labels <- make_labels(label)
#'   
#'   bugs_fn <- customBugs(bugs_params$PROG)
#'   
#'   ## run bugs model ----
#'   
#'   if (run_bugs) {
#'     init_vals <- map(1:bugs_params$N.CHAINS, ~dat$inits())
#'     
#'     res_bugs <-
#'       bugs_fn(
#'       # R2WinBUGS::bugs(
#'       # R2jags::jags(
#'         data = dat$bugsData,
#'         parameters.to.save = params_to_save,
#'         model.file = bugs_filename,
#'         n.chains = bugs_params$N.CHAINS,
#'         inits = init_vals,
#'         n.iter =
#'           (bugs_params$N.SIMS * bugs_params$N.THIN) + bugs_params$N.BURNIN,
#'         n.burnin = bugs_params$N.BURNIN,
#'         n.thin = bugs_params$N.THIN
#'         #codaPkg=FALSE
#'       )
#'     
#'     if (bugs_params$PROG == "JAGS")
#'       res_bugs <- res_bugs$BUGSoutput
#'   } else {
#'       file_name <- paste0("bugsObject_", label, ".pdf")
#'        load(file.path(output_dir, "model", file_name))
#'   }
#'   
#'   ## process output ----
#' 
#'   createFolders(folder = output_dir,
#'                 "results", "graphs", "model", "sims", "data", "diagnostics")
#'   save_bugs_files(res_bugs, bugs_params, run_bugs, labels, output_dir)
#'   
#'   if (DIAGNOSTICS)
#'      diagnostic_plots(res_bugs, labels)
#'   
#'   bugs_stats(dat, res_bugs, effectParam, random)
#'   
#'   plots_and_tables(dat, res_bugs, effectParam, labels, endpoint)
#'   
#'   file_name <- paste0("bugsObject_", label, ".RData")
#'   save(res_bugs, file.path(output_dir, "model", file_name))
#'        
#'   return(res_bugs)
#' }
#' 
#'  