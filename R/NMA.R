
#' Network meta-analysis
#' 
#' @param winSource
#' @param dataFunc 
#' @param effectParam 
#' @param modelParams 
#' @param folder 
#' @param label 
#' @param endpoint 
#' @param binData 
#' @param medData 
#' @param refTx 
#' @param preRefTx 
#' @param decEff 
#' @param random 
#' @param lg 
#' 
#' @return
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
  
  SYS <- .Platform$OS.type
  
  ## Short label without spaces
  slabel <- sub(" ", "_", label)
  
  ## Short label without spaces and FE/RE for data file
  sdlabel  <-
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
    if (SYS == "windows" & RUN) {
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
    Dev <- Deviance
    
    if (random) {
      SD <-
        round(x$summary[grep(paste0("^", "sd"),
                             rownames(x$summary)), c(1, 5, 2, 3, 7, 8)], 2)
    }
    
    para <-  
      if (random) {
        rbind(Deviance, SD)
      } else {
        rbind(Deviance)
      }
    
    colnames(para) <- paste(colEff)
    
    resDev <- x$summary["totresdev", "mean"]
    DIC <- x$DIC
    
    results <-
      list(EffectRes_lhr, para, DIC = DIC, resDev = resDev)
    
  }
  
  ## plots and table ----
  # plot_and_tables()

  # global variables  
 #  resDev
 #  Dev
 # slabel
 # sdlabel
  
  
  
  return(subData)
}

