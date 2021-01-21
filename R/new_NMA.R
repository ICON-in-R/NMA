
#' nma constructor
#'
#' @param subData 
#' @param subDataMed 
#' @param subDataBin 
#' @param bugs_params 
#' @param is_random 
#' @param refTx 
#' @param effectParam 
#' @param modelParams 
#' @param label 
#' @param endpoint 
#'
#' @return
#' @export
#'
new_NMA <- function(subData,
                    subDataMed = NA,
                    subDataBin = NA,
                    bugs_params = NA,
                    is_random = TRUE,
                    refTx = NA ,
                    effectParam,
                    modelParams,
                    label,
                    endpoint) {
  
  is_med <- ifelse(is.na(subDataMed), FALSE, TRUE)
  is_bin <- ifelse(is.na(subDataBin), FALSE, TRUE)
  
  bugs_params <- 
    modifyList(bugs_params,
               list(PROG = "openBugs",
                    N.BURNIN = 10,
                    N.SIMS = 150,
                    N.CHAINS = 2,
                    N.THIN = 1,
                    PAUSE = TRUE,
                    run_bugs = TRUE))
  
  bugs_fn <- customBugs(bugs_params$PROG)
  
  dat <- 
    setupData(subData = subData,
              subDataMed = subDataMed,
              subDataBin = subDataBin,
              is_random = is_random,
              refTx = refTx)
  
 structure(list(.call = as.list(match.call()),
                dat = dat,
                is_med = is_med,
                is_bin = is_bin,
                bugs_params = bugs_params,
                bugs_fn = bugs_fn,
                is_random = is_random,
                refTx = refTx,
                effectParam = effectParam,
                modelParams = modelParams,
                label = label,
                endpoint = endpoint),
           class = "nma") 
}

