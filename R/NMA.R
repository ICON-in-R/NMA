
#'
NMA <- function(winSource,
                dataFunc,
                effectParam = NA,
                modelParams = NA,
                folder,
                label,
                endpoint,
                binData,
                medData,
                refTx = NA,
                preRefTx = NA,
                decEff,
                random = FALSE,
                lg) {
  
  ## Short label without spaces
  slabel <<- sub(" ", "_", label)
  
  ## Short label without spaces and FE/RE for data file
  sdlabel  <<-
    sub(" ", "_", gsub("\\_RE", "", (gsub("\\_FE", "", label))))
  
  ## create folders
  createFolders(folder = folder, "results", "graphs", "model", "sims", "data")
  
  z <- dataFunc
  
  all_params <- c(effectParam, modelParams)
  params_to_save <- all_params[!is.na(all_params)]
  
  if (RUN) {
    x <-
      try(genSamps(
        newBugs.file = winSource,
        bugsData = bugsData,
        inits = inits,
        parameters.to.save = params_to_save,
        folder = folder))
  } else {
    load(file = paste0(
      folder,
      fileSep,
      "model",
      fileSep,
      "bugsObject_",
      slabel))
  }
  
  if (class(x) != "try-error") {
    if (SYS == "WIN" & RUN) {
      for (ii in seq_len(N.CHAINS)) {
        system(paste(
          Sys.getenv("COMSPEC"),
          "/c",
          paste0(
            "copy ",
            tempdir(),
            fileSep,
            "inits",
            ii,
            ".txt ",
            folder,
            fileSep,
            "model",
            fileSep,
            slabel,
            "_inits",
            ii,
            ".txt")
        ))
      }
      
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0(
          "copy ",
          tempdir(),
          fileSep,
          "data.txt ",
          folder,
          fileSep,
          "model",
          fileSep,
          slabel,
          "_data.txt")
      ))
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0(
          "copy ",
          tempdir(),
          fileSep,
          winSource,
          " ",
          folder,
          fileSep,
          "model",
          fileSep,
          slabel,
          "_",
          winSource)
      ))
    }
    
    save(file = paste0(
      folder,
      fileSep,
      "model",
      fileSep,
      "bugsObject_",
      slabel),
      x)
    
    dummyOR <- c(1, 1, NA, 1, 1, NA)
    dummy <- c(0, 0, NA, 0, 0, NA)
    colEff <- c("Mean", "Median", "SE", "L95CrI", "U95CrI", "Rhat")
    numEffectParam <- length(effectParam)
    
    effectParamName <-
      c("Log Hazard Ratio", "Parameters", "DIC", "Residual Deviance")
    
    EffectRes_lhr <-
      resultsFileSetUp(x$summary, "beta", dummy, dummyOR, colEff)
    
    para <-
      round(x$summary[-grep(paste0("^", effectParam[1]),
                            rownames(x$summary)), c(1, 5, 2, 3, 7, 8)], 2)
    
    if (numEffectParam > 1) {
      for (ee in 2:length(effectParam)) {
        para <-
          para[-grep(paste0("^", effectParam[ee]), rownames(para)),]
        
      }
    }
    Deviance <-
      round(x$summary[grep(paste0("^", "deviance"),
                           rownames(x$summary)), c(1, 5, 2, 3, 7, 8)], 2)
    Dev <<- Deviance
    
    if (random) {
      SD <-
        round(x$summary[grep(paste0("^", "sd"),
                             rownames(x$summary)), c(1, 5, 2, 3, 7, 8)], 2)
    }
    
    para <-  
      if (random) {
        rbind(Deviance , SD)
      } else {
        rbind(Deviance)
      }
    colnames(para) <- paste(colEff)
    
    resDev <<- x$summary["totresdev", "mean"]
    DIC <- x$DIC
    
    results <-
      list(EffectRes_lhr, para, DIC = DIC, resDev = resDev)
    
    if (!is.na(effectParam)) {
      simsLHR <-
        x$sims.matrix[, grep(paste0("^beta"), rownames(x$summary))]
      simsLHR <- cbind(0, simsLHR)
      colnames(simsLHR) <- txList
      sims <<- simsLHR
      
      # rank probability plot
      rankFileLoc <<-
        paste0(folder,
               fileSep,
               "graphs",
               fileSep,
               "ranking_",
               slabel,
               ".pdf")
      rankProbPlot(sims)
      newSavePlot(file = rankFileLoc)
      
      # forest plot
      
      forestFileLoc <<-
        paste0(folder,
               fileSep,
               "graphs",
               fileSep,
               "forest_",
               slabel,
               ".pdf")
      txEffectPlot(sims)
      newSavePlot(file = forestFileLoc)
      
      # pairwise table
      
      pairTable <<- pairwiseTable(sims = sims)
      pairFileLoc <<-
        paste0(folder,
               fileSep,
               "results",
               fileSep,
               "Pairwise_results_",
               slabel,
               ".csv")
      
      write.table(
        paste(
          "Pairwise Treatment Co-efficients;",
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
    
    # network diagramslabel
    
    layout(1)
    networkFileLoc <<-
      paste0(folder,
             fileSep,
             "graphs",
             fileSep,
             "netGraph_",
             slabel,
             ".pdf")
    
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
    newSavePlot(file = networkFileLoc)
    
    # data table
    
    dataTableLong <- subData
    
    dataFileLoc <<-
      paste0(folder,
             fileSep,
             "data",
             fileSep,
             "data_",
             sdlabel,
             ".csv")
    
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
    
    if (binData) {
      dataTableLongBin <- subDataBin
      
      dataFileLocBin <<-
        paste0(folder,
               fileSep,
               "data",
               fileSep,
               "data_",
               sdlabel,
               "_bin.csv")
      
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
        dataTableLongBin,
        file = dataFileLocBin,
        sep = ",",
        append = TRUE,
        col.names = NA)
    }
    
    if (medData) {
      dataTableLongMed <- subDataMed
      
      dataFileLocMed <<-
        paste0(folder,
               fileSep,
               "data",
               fileSep,
               "data_",
               sdlabel,
               "_med.csv")
      
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
        dataTableLongMed,
        file = dataFileLocMed,
        sep = ",",
        append = TRUE,
        col.names = NA)
    }
    
    # results table
    
    resultsFileLoc <<-
      paste0(folder,
             fileSep,
             "results",
             fileSep,
             "nmaResults_",
             slabel,
             ".csv")
    
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
  }
  
  return(subData)
}

