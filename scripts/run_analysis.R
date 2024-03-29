
# N Green, UCL 7-12-2020 --------------------------------------------------

library(dplyr)
library(purrr)


## settings

bugs_params <-
  list(
    PROG = "openBugs",
    N.BURNIN = 10,#00,
    N.SIMS = 150,#0,
    N.CHAINS = 2,
    N.THIN = 1,
    PAUSE = TRUE)

RUN <- TRUE
DIAGNOSTICS <- TRUE

saveplot_fn <- customSavePlot()


## run analysis

analyses_params <-
  read.csv(
    here::here("raw_data", "AnalysisList.csv"),
    as.is = TRUE,
    na.strings = c("NR", "NA")) %>% 
  filter(Endpoint_type == "Surv") %>% 
  dplyr::rename(name = Analysis_name,
                type = Analysis_Type)

# for (a in 1:nrow(analysis)) {
a <- 1

analysis <- analyses_params[a, ]

# fixed effects RANDOM=FALSE, random effects RANDOM=TRUE
RANDOM <- analysis$Model_effects == "RE"

REFTX <- analysis$REFTX

# indicator for availability of binary endpoint data
is_bin <- analysis$BinData == "YES"

# indicator for availability of median endpoint data
is_med <- analysis$MedData == "YES"

#}


# read in datasets

filename <- paste0(here::here("raw_data"), "/survdata_", analysis$Endpoint, "_")

subData <-
  read.csv(paste0(file_name, analysis$type, ".csv"),
           header = TRUE,
           as.is = TRUE)

if (is_bin) {
  survDataBin <-
    read.csv(paste0(file_name, "bin.csv"),
             header = TRUE,
             as.is = TRUE)
}

if (is_med) {
  survDataMed <-
    read.csv(paste0(file_name, "med.csv"),
             header = TRUE,
             as.is = TRUE) %>% 
    mutate(medR = floor(medR))
}

  
nma_res <-
  setupData(subData = subData,
            survDataMed = survDataMed,
            survDataBin = survDataBin,
            is_random = RANDOM,
            refTx = REFTX) %>% 
  NMA(dat = .,
      bugs_params = bugs_params,
      effectParam = "beta",
      modelParams = "totresdev",
      label = analysis$name,
      endpoint = analysis$Endpoint,
      random = RANDOM)

#}


#########
# plots #
#########

library(sna)
dat <- 
  setupData(subData = subData,
            survDataMed = survDataMed,
            survDataBin = survDataBin,
            is_random = RANDOM,
            refTx = REFTX) 

plotNetwork(dat)

