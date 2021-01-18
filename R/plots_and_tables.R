
#' plots_and_table
#'
#' @param dat 
#' @param effectParam 
#' @param labels 
#' @param fileSep 
#'
#' @return
#' @export
#'
plots_and_table <- function(dat,
                            effectParam,
                            labels,
                            fileSep = "/") {
  
  if (!is.na(effectParam)) {
    simsLHR <-
      x$sims.matrix[, grep(paste0("^beta"), rownames(x$summary))]
    simsLHR <- cbind(0, simsLHR)
    colnames(simsLHR) <- txList
    sims <- simsLHR
    
    # rank probability plot
    rankFileLoc <- paste0(folder, fileSep, "graphs", fileSep, "ranking_", labels$short, ".pdf")
    
    rankProbPlot(sims)
    
    # newSavePlot(file = rankFileLoc)
    pdf(file = rankFileLoc)
    rankProbPlot(sims)
    dev.off()
    
    # forest plot
    
    forestFileLoc <- paste0(folder, fileSep, "graphs", fileSep, "forest_", labels$short, ".pdf")
    
    txEffectPlot(sims, preRefTx, refTx)
    
    # newSavePlot(file = forestFileLoc)
    pdf(file = forestFileLoc)
    rankProbPlot(sims)
    dev.off()
    
    # pairwise table
    
    pairTable <- pairwiseTable(sims = sims)
    pairFileLoc <- paste0(folder, fileSep, "results", fileSep, "Pairwise_results_", labels$short, ".csv")
    
    write.table(
      paste("Pairwise Treatment Co-efficients;",
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
  
  # network diagramlabels$short
  
  layout(1)
  networkFileLoc <-
    paste0(folder, fileSep, "graphs", fileSep, "netGraph_", labels$short, ".pdf")
  
  par(mar = c(3, 3, 3, 3))
  plotNetwork(
    subData = subData,
    subDataBin = subDataBin,
    binData = binData,
    subDataMed = subDataMed,
    medData = medData,
    mode = "fruchtermanreingold",
    label.pos = 0,
    vertex.enclose = TRUE,
    pad = 1,
    label.cex = 0.7,
    vertex.cex = 1)
  
  # newSavePlot(file = networkFileLoc)
  pdf(file = networkFileLoc)
  rankProbPlot(sims)
  dev.off()
  
  # data table
  
  dataTableLong <- subData
  
  dataFileLoc <-
    paste0(folder, fileSep, "data", fileSep, "data_", sdlabel, ".csv")
  
  write.table(
    paste0(
      "Key: Lmean=mean log hazard ratios; Lse=standard error for log hazard ratios; multi=multi-arm trial indicator"),
    file = dataFileLoc ,
    append = FALSE,
    col.names = NA)
  
  write.table(
    "------------------",
    file = dataFileLoc,
    append = TRUE,
    col.names = NA)
  
  write.table(
    dataTableLong,
    file = dataFileLoc,
    sep = ",",
    append = TRUE,
    col.names = NA)
  
  if (!is.na(dat$survDatBin)) {
    
    dataFileLocBin <- paste0(folder, fileSep, "data", fileSep, "data_", sdlabel, "_bin.csv")
    
    write.table(
      paste0(
        "Key: Bn=Number of patients in arm; Br=number of events in arm"),
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
  
  if (!is.na(dat$survDatMed)) {
    
    dataFileLocMed <- paste0(folder, fileSep, "data", fileSep, "data_", sdlabel, "_med.csv")
    
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
  
  resultsFileLoc <- paste0(folder, fileSep, "results", fileSep, "nmaResults_", labels$short, ".csv")
  
  write.table(
    paste(
      "Model Co-efficients:",
      "treatment effects compared to",
      refTx,
      sep = " "),
    file = resultsFileLoc,
    append = FALSE)
  
  write.table(
    paste0(
      "Key: SE=standard error; L95CrI/U95CrI=lower/upper 95% credible interval; DIC=deviance information criterion"),
    file = resultsFileLoc,
    append = TRUE,
    col.names = NA)
  
  write.table(
    "------------------",
    file = resultsFileLoc ,
    append = TRUE,
    col.names = NA)
  
  for (i in seq_along(results)) {
    write.table(
      effectParamName[i],
      file = resultsFileLoc,
      sep = ",",
      append = TRUE,
      col.names = NA)
    
    write.table(
      results[i],
      file = resultsFileLoc,
      sep = ",",
      append = TRUE,
      col.names = NA)
  }
  
  invisible(bugs_res)
}

