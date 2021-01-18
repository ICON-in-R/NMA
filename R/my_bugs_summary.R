
#'
summStat <- function(x) {
  y <- c(mean(x), quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE))
  names(y)[1] <- "mean"
  return(y)
}


#' my_bugs_summary
#'
#' Function to set up results file for
#' multiple parameters monitored
#' 
#' @param bugs_summary 
#' @param params 
#' @param dummy 
#' @param dummyOR 
#' @param colEff 
#'
#' @import dplyr
#' @return
#' @export
#'
my_bugs_summary <- function(bugs_summary,
                            params) {#,
  # dummy,
  # dummyOR,
  # colEff) {
  
  keep_cols <- c("mean", "50%", "sd", "2.5%", "97.5%", "Rhat")
  keep_rows <- grep(paste("^", params, sep = ""), rownames(bugs_summary))
  
  eff <-
    round(bugs_summary[keep_rows, keep_cols], 2)
  
  # remove dev row
  eff <- eff[!grepl(paste("^", "dev", sep = ""), rownames(eff)), ]
  
  
  ##TODO: inlcude this information  
  # if (length(txList) != nrow(eff)) {
  #   if (params != "hr") {
  #     eff <- rbind(dummy, eff)
  #   } else{
  #     eff <- rbind(dummyOR, eff)
  #   }
  # }
  # colnames(eff) <- paste(colEff)
  # rownames(eff)  <- txList
  
  return(eff)
}


