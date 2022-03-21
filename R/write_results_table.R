
#' Write results table
#' 
#' @param nma
#' @param ... Additional arguments
#' @export
#'
write_results_table <- function(nma,
                                res_bugs,
                                folder = "output",
                                label = "",
                                ...) {
  dat <- nma$dat
  
  file_name <- paste0("nmaResults_", label, ".csv")
  dir_name <- file.path(folder, "results", file_name)
  
  writeLines(
    paste(
      "Model Co-efficients:",
      "treatment effects compared to", dat$txList[1], "\n",
      "Key: SE=standard error;
            L95CrI/U95CrI=lower/upper 95% credible interval;
            DIC=deviance information criterion\n",
    "------------------"),
    dir_name)
  
  #TODO: what is this doing?...
  # for (i in seq_along(effectParamName)) {
  #   write.table(
  #     effectParamName[i],
  #     file = dir_name,
  #     sep = ",",
  #     append = TRUE,
  #     col.names = NA)
  # 
  #   write.table(
  #     res_bugs[i],
  #     file = dir_name,
  #     sep = ",",
  #     append = TRUE,
  #     col.names = NA)
  # }
}

