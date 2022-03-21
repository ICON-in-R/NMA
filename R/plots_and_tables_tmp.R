
#' Write to file multiple NMA plots and tables
#'
#' @param dat List of study data, including subData
#'  and possibly subDataBin and subDataMed
#' @param res_bugs BUGS output
#' @param effectParam Effect parameter names; string
#' @param labels Labels
#' @param endpoint End point names; string
#' @param folder Output folder name; string
#' @param fileSep File separator; default forward slash
#'
#' @return res_bugs
#' @export
#'
plots_and_tables <- function(dat,
                             res_bugs,
                             effectParam,
                             labels,
                             endpoint = NULL,
                             folder = "output",
                             fileSep = "/") {
  
  # what does this mean?
  if (all(!is.na(effectParam))) {
    
    plot_dat <-
      list(dat = dat,
           res_bugs = res_bugs,
           labels = labels,
           endpoint = endpoint)
    
    plot_fns <-
      list(rankProbPlot,
           txEffectPlot,
           pairwiseTable)
    
    plot_fns %>% 
      map(do.call, plot_dat)
  }
  
  write_data_to_file(dat, labels, fileSep)
  
  write_results_table(dat, labels, fileSep)
  
  invisible(res_bugs)
}

