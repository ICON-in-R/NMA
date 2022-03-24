
#' @keywords internal
#' 
diagnostics <- function(res_bugs,
                        labels_short = "",
                        output_dir = "output") {
  
  createFolders(folder = output_dir, "diagnostics")
  diagnostic_plots(res_bugs, labels_short)
  
  invisible(res_bugs)
}


#' All output functions
#' @keywords internal
#' 
nma_outputs <- function(nma,
                        nma_model,
                        output_dir = "output") {
 
  createFolders(folder = output_dir, "graphs")
  
  bugs_stats(nma_model$dat, nma, nma_model$effectParam, nma_model$is_random)
  
  plots_and_tables(dat = nma_model$dat,
                   res_bugs = nma,
                   effectParam = nma_model$effectParam,
                   endpoint = nma_model$endpoint)
  
  invisible(nma)
}

