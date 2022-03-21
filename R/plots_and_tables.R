
#' Write to file multiple NMA plots and tables
#'
#' @param dat ist of study data, including subData
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
                             labels = list(short = "",
                                           refe = "",
                                           orig = ""),
                             endpoint = NULL,
                             folder = "output",
                             fileSep = "/") {
  
  if (all(!is.na(effectParam))) {
    
    sims <-
      res_bugs$sims.matrix[, grep(paste0("^beta"), rownames(res_bugs$summary))]
    sims <- cbind(0, sims)
    colnames(sims) <- dat$txList
    
    # rank probability plot
    
    rankFileLoc <-
      paste0(folder, fileSep, "graphs", fileSep, "ranking_", labels$short, ".pdf")
    
    rankProbPlot(sims, labels)
    
    pdf(file = rankFileLoc)
    rankProbPlot(sims, labels)
    dev.off()
    
    # forest plot
    
    forestFileLoc <-
      paste0(folder, fileSep, "graphs", fileSep, "forest_", labels$short, ".pdf")

    txEffectPlot(dat, sims, labels, endpoint)

    pdf(file = forestFileLoc)
    txEffectPlot(dat, sims, labels, endpoint)
    dev.off()
    
    # pairwise table
    
    pairTable <- pairwiseTable(sims = sims)
    pairFileLoc <-
      paste0(folder, fileSep, "results", fileSep, "Pairwise_results_", labels$short, ".csv")
    
    write.table(
      paste("Pairwise Treatment Coefficients;",
            "Median hazard ratio (95% Credible Interval)",
            sep = " "),
      file = pairFileLoc ,
      append = FALSE)
    
    write.table(
      "Row treatment vs. Column treatment (reference)",
      file = pairFileLoc ,
      append = TRUE,
      col.names = NA)
    
    write.table(
      "------------------",
      file = pairFileLoc ,
      append = TRUE,
      col.names = NA)
    
    write.table(
      pairTable,
      file = pairFileLoc,
      sep = ",",
      append = TRUE,
      col.names = NA)
  }

  # data table
  
  dataFileLoc <-
    paste0(folder, fileSep, "data", fileSep, "data_", labels$refe, ".csv")
  
  write.table(
    paste0(
      "Key: Lmean = mean log hazard ratios;
            Lse = standard error for log hazard ratios;
            multi = multi-arm trial indicator"),
    file = dataFileLoc,
    append = FALSE,
    col.names = NA)
  
  write.table(
    "------------------",
    file = dataFileLoc,
    append = TRUE,
    col.names = NA)
  
  write.table(
    dat$subData,
    file = dataFileLoc,
    sep = ",",
    append = TRUE,
    col.names = NA)
  
  if (all(!is.na(dat$subDataBin))) {
    
    dataFileLocBin <-
      paste0(folder, fileSep, "data", fileSep, "data_", labels$refe, "_bin.csv")
    
    write.table(
      paste0(
        "Key: Bn is number of patients in arm; Br is number of events in arm"),
      file = dataFileLocBin,
      append = FALSE,
      col.names = NA)
    
    write.table(
      "------------------",
      file = dataFileLocBin,
      append = TRUE,
      col.names = NA)
    
    write.table(
      dat$subDataBin,
      file = dataFileLocBin,
      sep = ",",
      append = TRUE,
      col.names = NA)
  }
  
  if (all(!is.na(dat$subDataMed))) {
    
    dataFileLocMed <-
      paste0(folder, fileSep, "data", fileSep, "data_", labels$refe, "_med.csv")
    
    write.table(
      paste0("Key: ..."),
      file = dataFileLocMed,
      append = FALSE,
      col.names = NA)
    
    write.table(
      "------------------",
      file = dataFileLocMed,
      append = TRUE,
      col.names = NA)
    
    write.table(
      dat$subDataMed,
      file = dataFileLocMed,
      sep = ",",
      append = TRUE,
      col.names = NA)
  }
  
  # results table
  
  resultsFileLoc <-
    paste0(folder, fileSep, "results", fileSep, "nmaResults_", labels$short, ".csv")
  
  write.table(
    paste(
      "Model Co-efficients:",
      "treatment effects compared to",
      dat$txList[1],
      sep = " "),
    file = resultsFileLoc,
    append = FALSE)
  
  write.table(
    paste0(
      "Key: SE = standard error;
            L95CrI/U95CrI = lower/upper 95% credible interval;
            DIC = deviance information criterion"),
    file = resultsFileLoc,
    append = TRUE,
    col.names = NA)
  
  write.table(
    "------------------",
    file = resultsFileLoc ,
    append = TRUE,
    col.names = NA)

  ##TODO: what is this doing?...  
  # for (i in seq_along()) {
  #   write.table(
  #     effectParamName[i],
  #     file = resultsFileLoc,
  #     sep = ",",
  #     append = TRUE,
  #     col.names = NA)
  #   
  #   write.table(
  #     res_bugs[i],
  #     file = resultsFileLoc,
  #     sep = ",",
  #     append = TRUE,
  #     col.names = NA)
  # }
  
  invisible(res_bugs)
}

