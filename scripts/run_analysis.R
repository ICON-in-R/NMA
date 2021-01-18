
# N Green, UCL 7-12-2020 --------------------------------------------------

library(dplyr)
library(purrr)


## settings

bugs_params <-
  list(
    PROG = "openBugs",
    N.BURNIN = 1000,
    N.SIMS = 1500,
    N.CHAINS = 2,
    N.THIN = 1,
    PAUSE = TRUE)

RUN <- TRUE
DIAGNOSTICS <- TRUE
BASEPROB <- NA
decEff <- TRUE

saveplot_fn <- customSavePlot()
bugs_fn <- customBugs()


## run analysis

analysis <-
  read.csv(
    here::here("raw_data", "AnalysisList.csv"),
    as.is = TRUE,
    na.strings = c("NR", "NA"))

analysis <- filter(analysis, Endpoint_type == "Surv")


# for (a in 1:nrow(analysis)) {
a <- 1

print(analysis$Analysis_name[a])

currAnalysis <- analysis[a, ]

treatName <- currAnalysis$Comparators

analysis_desc <- currAnalysis$analysis_desc

analysis_type <- currAnalysis$Analysis_Type

# for fixed effects RANDOM=FALSE, for random effects RANDOM=TRUE
RANDOM <- currAnalysis$Model_effects == "RE"

label <- currAnalysis$Analysis_name

endpoint <- currAnalysis$Endpoint

endpoint_type <- currAnalysis$Endpoint_type

decEff <- currAnalysis$Decrease_Effect

REFTX <- currAnalysis$REFTX

# indicator for availability of binary endpoint data
is_bin <- currAnalysis$BinData == "YES"

# indicator for availability of median endpoint data
is_med <- currAnalysis$MedData == "YES"


#}


# read in datasets
subData <-
  read.csv(
    paste0(here::here("raw_data"), "/survdata_", endpoint, "_", analysis_type, ".csv"),
    header = TRUE,
    as.is = TRUE)

if (is_bin) {
  subDataBin <-
    read.csv(
      paste0(here::here("raw_data"), "/survdata_", endpoint, "_bin.csv"),
      header = TRUE,
      as.is = TRUE)
}

if (is_med) {
  subDataMed <-
    read.csv(
      paste0(here::here("raw_data"), "/survdata_", endpoint, "_med.csv"),
      header = TRUE,
      as.is = TRUE)
}


modelResults <-
  setupData(subData = subData,
            subDataMed = subDataMed,
            subDataBin = subDataBin,
            is_random = RANDOM,
            refTx = REFTX) %>% 
  NMA(dat = .,
      bugs_params = bugs_params,
      bugs_fn = bugs_fn,
      effectParam = "beta",
      modelParams = "totresdev",
      label = label,
      endpoint = endpoint,
      random = RANDOM)

#}

#########
# plots #
#########

plotNetwork(subData,
            subDataBin, is_bin,
            subDataMed, is_med)

