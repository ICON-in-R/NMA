
summStat <- function(x) {
  y <- c(mean(x), quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE))
  names(y)[1] <- "mean"
  return(y)
}


# Function to set up results file for multiple parameters monitored
resultsFileSetUp <- function(resultsFile,
                             effectParam,
                             dummy,
                             dummyOR,
                             colEff) {
  eff <-
    round(resultsFile[grep(paste("^", effectParam, sep = ""),
                           rownames(resultsFile)), c(1, 5, 2, 3, 7, 8)], 2)
  eff <- eff[!grepl(paste("^", "dev", sep = ""), rownames(eff)),]
  #eff <- eff[!grepl(paste("^","Ldev",sep=""),rownames(eff)),]
  
  if (length(txList) != nrow(eff)) {
    if (effectParam != "hr") {
      eff <- rbind(dummy, eff)
    } else{
      eff <- rbind(dummyOR, eff)
    }
  }
  colnames(eff) <- paste(colEff)
  rownames(eff)  <- txList
  return(eff)
}


