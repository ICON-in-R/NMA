
test_that("new_NMA", {
  
  bugs_params <-
    list(
      PROG = "openBugs",  # which version of BUGS to use to run the MCMC
      N.BURNIN = 10,#00,  # number of steps to throw away
      N.SIMS = 150,#0,    # total number of simulations
      N.CHAINS = 2,       # number of chains
      N.THIN = 1,         # thinning rate
      PAUSE = TRUE)
  
  REFTX <- "X"                # reference treatment
  label_name <- "label_name"
  
  # file_name <- here::here(file.path("inst", "extdata", "survdata_"))
  file_name <- "C:/Users/n8tha/Documents/R/NMA/inst/extdata/survdata_"
  
  subDataHR <-
    read.csv(paste0(file_name, "hr_test.csv"),
             header = TRUE,
             as.is = TRUE)
  
  subDataBin <-
    read.csv(paste0(file_name, "bin_test.csv"),
             header = TRUE,
             as.is = TRUE)
  
  subDataMed <-
    read.csv(paste0(file_name, "med_test.csv"),
             header = TRUE,
             as.is = TRUE) %>% 
    mutate(medR = floor(medR))
  
  data_types <- c("hr_data", "bin_data", "med_data")
  
  nma_model <- list()
  
  # random or fixed effects models
  for (i in c(TRUE, FALSE)) {
    
    datasets <- combn(data_types, 2, simplify = FALSE)
    nma_model[[as.character(i)]] <-
      vector(mode = "list", length = length(datasets))
    
    # all pairs of types of survival data
    for (j in seq_along(datasets)) {
      
      nma_model[[as.character(i)]][[j]] <-
        new_NMA(subDataHR = subDataHR,
                subDataMed = subDataMed,
                subDataBin = subDataBin,
                bugs_params = bugs_params,
                is_random = i,
                data_type = datasets[[j]],
                refTx = REFTX ,
                effectParam = "beta",
                label = "",
                endpoint = "")
    }
  }
})


test_that("NMA_run", {
  
  # nma_res <- list()
  # 
  # for (i in c(TRUE, FALSE)) {
  #   
  #   nma_res[[as.character(i)]] <-
  #     vector(mode = "list", length = length(datasets))
  #   
  #   for (j in seq_along(datasets)) {
  #     nma_res[[as.character(i)]][[j]] <-
  #       NMA_run(nma_model[[as.character(i)]][[j]], save = FALSE)
  #     # print(nma_res[[as.character(i)]][[j]])
  #   }
  # }
})



# test if data not available

# test updating model runs
