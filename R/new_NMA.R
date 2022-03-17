
#' NMA constructor
#' 
#' Create an \code{nma} class object to use in an analysis.
#' 
#' @param subData Main input data frame. Mandatory
#' @param subDataMed Median time input data frame. Optional
#' @param subDataBin Binary data input data frame. Optional
#' @param bugs_params List of BUGS parameters. Optional
#' @param is_random Random effects model? Logical
#' @param hyperparams List of hyperparameters
#' @param refTx Reference treatment; string
#' @param effectParam Effect parameter
#' @param modelParams Model parameter
#' @param label Label
#' @param endpoint End point name; string
#' @seealso \code{\link{NMA_run}}, \code{\link{NMA_update}}
#' @return
#' @export
#'
new_NMA <- function(subData,
                    subDataMed = NA,
                    subDataBin = NA,
                    bugs_params = NA,
                    is_random = TRUE,
                    hyperparams = list(),
                    refTx = NA ,
                    effectParam,
                    modelParams,
                    label,
                    endpoint) {
  
  is_med <- ifelse(any(is.na(subDataMed)), FALSE, TRUE)
  is_bin <- ifelse(any(is.na(subDataBin)), FALSE, TRUE)
  
  bugs_params <- 
    modifyList(list(PROG = "openBugs",
                    N.BURNIN = 10,
                    N.SIMS = 150,
                    N.CHAINS = 2,
                    N.THIN = 1,
                    PAUSE = TRUE,
                    run_bugs = TRUE),
               bugs_params,)
  
  bugs_fn <- customBugs(bugs_params$PROG)
  
  check_study_data(subData, subDataMed, subDataBin)
  
  dat <- 
    setupData(subData = subData,
              subDataMed = subDataMed,
              subDataBin = subDataBin,
              is_random = is_random,
              refTx = refTx)
  
  dat$bugsData <-
    modifyList(list(mu_beta = 0,
                    prec_beta = 1.0E-6,
                    mu_alpha = 0,
                    prec_alpha = 1.0E-6),
               c(dat$bugsData, hyperparams))
  
  structure(list(dat = dat,
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
            class = "nma",
            CALL = as.list(match.call()[-1])) 
}

