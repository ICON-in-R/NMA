
# non-survival data

data_type <- "conts_data"

file_name <- paste0(data_type, "_test.csv")

nma_data <-
  read.csv(file.path("inst", "extdata", file_name),
           header = TRUE,
           as.is = TRUE)

nma_model <-
  new_NMA(
    contsData = nma_data,
    bugs_params = bugs_params,
    is_random = RANDOM,
    data_type = data_type,
    refTx = REFTX,
    label = "",
    endpoint = "")

nma_model

nma_res <- NMA_run(nma_model, save = FALSE)

nma_res


