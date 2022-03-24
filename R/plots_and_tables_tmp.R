
#' Write to file multiple NMA plots and tables
#'
#' @param nma
#' @param res_bugs BUGS output
#' @param effectParam Effect parameter names; string
#' @param label Label
#' @param save Logical
#' @param endpoint End point names; string
#' @param folder Output folder name; string
#'
#' @return res_bugs
#' @export
#'
plots_and_tables <- function(nma,
                             res_bugs,
                             effectParam = NA,
                             label = "",
                             endpoint = NULL,
                             folder = "output",
                             save = FALSE) {
  plot_dat <-
    list(nma = nma,
         res_bugs = res_bugs,
         folder = folder,
         label = labels,
         save = save,
         endpoint = endpoint)
  
  # what does this mean?
  if (all(!is.na(effectParam))) {
    
    plot_fns <-
      list(rankProbPlot,
           txEffectPlot,
           pairwiseTable)
    
    plot_fns %>% 
      map(do.call, plot_dat)
  }
  
  do.call(write_data_to_file, plot_dat)
  do.call(write_results_table, plot_dat)
  
  invisible(res_bugs)
}

