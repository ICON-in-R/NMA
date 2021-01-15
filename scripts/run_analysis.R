
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

analysis_description <- currAnalysis$Analysis_description

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


# read in dataset
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


NMA_partial <-
  purrr::partial(NMA,
                 dat =
                   setupData(
                     subData = subData,
                     subDataBin = subDataBin,
                     subDataMed = subDataMed,
                     is_random = RANDOM,
                     refTx = REFTX),
                 effectParam = "beta",
                 folder = endpoint,
                 label = label,
                 endpoint = endpoint,
                 random = RANDOM,
                 decEff = decEff,
                 lg = FALSE)

if (!is_bin & !is_med) {
  modelResults <-
    if (RANDOM) {
      NMA_partial(
        bugs_filename = "SurvWoodsREb.txt",
        modelParams = c("sd", "totresdev"))
    } else {
      NMA_partial(
        bugs_filename = "SurvWoodsFEa.txt",
        modelParams = "totresdev")
    }
}

if (is_bin & !is_med) {
  if (!RANDOM) {
    modelResults <- NMA(
      bugs_filename = here::here("inst", "SurvWoodsFEa_bin.txt"),
      dat = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,        
        is_random = RANDOM,
        refTx = REFTX),
      effectParam = "beta",
      modelParams = NA,
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = RANDOM,
      decEff = decEff,
      lg = FALSE)
  } else {
    modelResults <- NMA(
      bugs_filename = here::here("inst", "SurvWoodsREb_bin.txt"),
      dat =
        setupData(
          subData = subData,
          subDataBin = subDataBin,
          subDataMed = subDataMed,        
          is_random = RANDOM,
          refTx = REFTX),
      effectParam = "beta",
      modelParams = "sd",
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      decEff = decEff,
      lg = FALSE)
  }
}

if (!is_bin & is_med) {
  if (!RANDOM) {
    modelResults <- NMA(
      bugs_filename = here::here("inst", "SurvWoodsFEa_med.txt"),
      dat = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,
        is_random = RANDOM,
        refTx = REFTX),
      effectParam = "beta",
      modelParams = NA,
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = FALSE,
      decEff = decEff,
      lg = FALSE)
  } else {
    modelResults <- NMA(
      bugs_filename = here::here("inst", "SurvWoodsREb_med.txt"),
      dat =
        setupData(
          subData = subData,
          subDataMed = subDataMed,
          subDataBin = subDataBin,
          is_random = RANDOM,
          refTx = REFTX),
      effectParam = "beta",
      modelParams = "sd",
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      decEff = decEff,
      lg = FALSE)
  }
}

if (is_bin & is_med) {
  if (!RANDOM) {
    modelResults <-
      NMA(bugs_filename = here::here("inst", "SurvWoodsFEa_med_bin.txt"),
          dat = setupData(
            subData = subData,
            subDataMed = subDataMed,
            subDataBin = subDataBin,
            is_random = RANDOM,
            refTx = REFTX),
          bugs_params = bugs_params,
          bugs_fn = bugs_fn,
          effectParam = "beta",
          modelParams = "totresdev",
          folder = endpoint,
          label = label,
          endpoint = endpoint,
          random = RANDOM,
          decEff = decEff,
          lg = FALSE)
  } else {
    modelResults <- NMA(
      bugs_filename = here::here("inst", "SurvWoodsREb_med_bin.txt"),
      dat = setupData(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin = subDataBin,
        is_random = RANDOM,
        refTx = REFTX),
      effectParam = "beta",
      modelParams = c("sd", "totresdev"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = RANDOM,
      decEff = decEff,
      lg = FALSE)
  }
}

#}

#########
# plots #
#########

plotNetwork(subData,
            subDataBin, is_bin,
            subDataMed, is_med)

