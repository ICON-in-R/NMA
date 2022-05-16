
test_that("test data", {
  
  bugs_params <-
    list(
      PROG = "openBugs",  # which version of BUGS to use to run the MCMC
      N.BURNIN = 10,#00,  # number of steps to throw away
      N.SIMS = 150,#0,    # total number of simulations
      N.CHAINS = 2,       # number of chains
      N.THIN = 1,         # thinning rate
      PAUSE = TRUE)
  
  RANDOM <- FALSE              # is this a random effects model?
  REFTX <- "X"                # reference treatment
  label_name <- "label_name"
  
  file_name <- system.file("extdata", "bin_data_test.csv", package = "NMA")
  
  nma_data <-
    read.csv(file_name,
             header = TRUE,
             as.is = TRUE)
  
  nma_model <-
    new_NMA(
      binData = nma_data,
      bugs_params = bugs_params,
      is_random = RANDOM,
      data_type = "bin_data",
      refTx = REFTX,
      label = "",
      endpoint = "")
  
  nma_model
  
  nma_res <- NMA_run(nma_model, save = FALSE)
  
  nma_res
})
  