
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
#' @param txList
#'
#' @import dplyr
#' @return
#' @export
#'
my_bugs_summary <- function(bugs_summary,
                            params,
                            txList) {
  
  keep_cols <- c("mean", "50%", "sd", "2.5%", "97.5%", "Rhat")
  keep_rows <- grep(paste("^", params, sep = ""), rownames(bugs_summary))
  
  eff <-
    round(bugs_summary[keep_rows, keep_cols], 2)
  
  # remove dev row
  eff <- eff[!grepl(paste("^", "dev", sep = ""), rownames(eff)), ]
  
  if (length(txList) != nrow(eff)) {
    if (params != "hr") {
      eff <- rbind(c(0, 0, NA, 0, 0, NA),
                   eff)
    } else{
      eff <- rbind(c(1, 1, NA, 1, 1, NA),
                   eff)
    }
  }
  rownames(eff) <- txList
  colnames(eff) <- c("Mean", "Median", "SE", "L95CrI", "U95CrI", "Rhat")
  
  return(eff)
}


