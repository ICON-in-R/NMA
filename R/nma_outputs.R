
#' @keywords internal
#' 
diagnostics <- function(res_bugs,
                        labels,
                        output_dir = "output",
                        fileSep = "/") {
  
  createFolders(folder = output_dir, fileSep, "diagnostics")
  diagnostic_plots(res_bugs, labels)
  
  invisible(res_bugs)
}


#' All output functions
#' @keywords internal
#' 
nma_outputs <- function(nma,
                        res_bugs,
                        effectParam,
                        labels,
                        endpoint,
                        output_dir = "output",
                        fileSep = "/") {
  
  createFolders(folder = output_dir, fileSep, "graphs")
  
  bugs_stats(nma$dat, res_bugs, effectParam, nma$is_random)
  
  plots_and_tables(nma$dat, res_bugs, effectParam, nma$labels, nma$endpoint)
  
  invisible(res_bugs)
}

