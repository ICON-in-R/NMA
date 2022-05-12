
#' NMA constructor
#' 
#' Create an \code{nma} class object to use in an analysis.
#' All of the data types are optional but at least one needs to be
#' passed to the function. The `data_type` must be a subset of 
#' the passed data sets.
#' 
#' @param subDataHR Hazard ratio input data frame. Optional
#' @param subDataMed Median time input data frame. Optional
#' @param subDataBin Survival binary data input data frame. Optional
#' @param binData Binary data input data frame. Optional
#' @param countData Count data input data frame. Optional
#' @param contsData Continuous data input data frame. Optional
#' @param bugs_params List of BUGS parameters. Optional
#' @param is_random Random effects model? Logical
#' @param data_type Vector of names of data formats from
#'                  "hr_data", "surv_bin_data", "med_data", "bin_data",
#'                  "count_data", "conts_data"
#' @param hyperparams List of hyperparameters
#' @param refTx Reference treatment; string
#' @param effectParam Effect parameter
#' @param label Label
#' @param endpoint End point name; string
#' @seealso \code{\link{NMA_run}}, \code{\link{NMA_update}}
#' @return
#' @export
#'
new_NMA <- function(subDataHR = NA,
                    subDataMed = NA,
                    subDataBin = NA,
                    binData = NA,
                    countData = NA,
                    contsData = NA,
                    bugs_params = NA,
                    is_random = TRUE,
                    data_type = NA,
                    hyperparams = list(),
                    refTx = NA ,
                    effectParam,
                    label,
                    endpoint) {

  data_lookup <-
    c(subDataHR = "hr_data",
      subDataMed = "surv_bin_data",
      subDataBin = "med_data",
      binData = "bin_data",
      countData = "count_data",
      contsData = "conts_data")
  
  data_type <-
    match.arg(data_type, unname(data_lookup),
              several.ok = TRUE)
  
  nma_datasets <-
    tibble::lst(subDataHR, subDataMed, subDataBin,
                binData, countData, contsData)
  
  # use all provided data
  if (any(is.na(data_type))) {
    avail_datasets <- nma_datasets[!is.na(nma_datasets)]
    data_type <- data_lookup[names(avail_datasets)]
  }
  
  model_params_lookup <-
    c(hr_data = "totLdev",
      surv_bin_data = NA,
      med_data = "totmediandev",
      bin_data = "totresdev")
  
  modelParams <- model_params_lookup[data_type]
  modelParams <- modelParams[!is.na(modelParams)]
  
  if (length(modelParams) > 1) modelParams <- c(modelParams, "totresdev") 
  
  bugs_params <- 
    modifyList(list(PROG = "openBugs",
                    N.BURNIN = 10,
                    N.SIMS = 150,
                    N.CHAINS = 2,
                    N.THIN = 1,
                    PAUSE = TRUE,
                    run_bugs = TRUE),
               bugs_params)
  
  bugs_fn <- customBugs(bugs_params$PROG)
  
  do.call(check_study_data, nma_datasets)
  
  dat <- 
    setupData(nma_datasets,
              data_type = data_type,
              is_random = is_random,
              refTx = refTx)
  
  dat$bugsData <-
    modifyList(list(mu_beta = 0,
                    prec_beta = 1.0E-6,
                    mu_alpha = 0,
                    prec_alpha = 1.0E-6),
               c(dat$bugsData, hyperparams))
  
  structure(list(dat = dat,
                 data_type = data_type,
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

