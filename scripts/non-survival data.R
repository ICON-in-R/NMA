
# non-survival data

bugs_params <-
  list(
    PROG = "openBugs",  # which version of BUGS to use to run the MCMC
    N.BURNIN = 10,#00,  # number of steps to throw away
    N.SIMS = 150,#0,    # total number of simulations
    N.CHAINS = 2,       # number of chains
    N.THIN = 1,         # thinning rate
    PAUSE = TRUE)

RANDOM <- FALSE             # is this a random effects model?
REFTX <- "X"                # reference treatment
label_name <- "label_name"

# which type of data to use
# data_type <- "count_data"
# data_type <- "bin_data"
data_type <- "conts_data"

file_name <- paste0(data_type, "_test.csv")

nma_data <-
  read.csv(file.path("inst", "extdata", file_name),
           header = TRUE,
           as.is = TRUE)

nma_model <-
  new_NMA(
    # countData = nma_data,
    # binData = nma_data,
    contsData = nma_data,
    bugs_params = bugs_params,
    is_random = RANDOM,
    data_type = data_type,
    refTx = REFTX ,
    effectParam = "beta",
    label = "",
    endpoint = "")

nma_model

nma_res <- NMA_run(nma_model, save = FALSE)

nma_res

