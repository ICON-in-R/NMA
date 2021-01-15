
# N Green, UCL 7-12-2020 --------------------------------------------------

library(dplyr)
library(purrr)


## settings

fileSep <- "\\"

RUN <- TRUE

N.BURNIN <- 1000
N.SIMS <- 1500
N.CHAINS <- 2
N.THIN <- 1
PAUSE <- TRUE

PROG <- "openBugs"
DIAGNOSTICS <- TRUE

BASEPROB <- NA
decEff <- TRUE

newSavePlot <- customSavePlot()
newBugs <- customBugs()

# run analysis

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

# selects indicator for availability of binary endpoint data
binDataInd <- currAnalysis$BinData

# selects indicator for availability of median endpoint data
medDataInd <- currAnalysis$MedData

binData <- binDataInd == "YES"
medData <- medDataInd == "YES"

#}


# read in dataset
subData <-
  read.csv(
    paste0(here::here("raw_data"), "/survdata_", endpoint, "_", analysis_type, ".csv"),
    header = TRUE,
    as.is = TRUE)

if (binData) {
  subDataBin <-
    read.csv(
      paste0(here::here("raw_data"), "/survdata_", endpoint, "_bin.csv"),
      header = TRUE,
      as.is = TRUE)
}

if (medData) {
  subDataMed <-
    read.csv(
      paste0(here::here("raw_data"), "/survdata_", endpoint, "_med.csv"),
      header = TRUE,
      as.is = TRUE)
}


NMA_partial <-
  purrr::partial(NMA,
                 dataFunc =
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
                 binData = binData,
                 medData = medData,
                 refTx = REFTX,
                 decEff = decEff,
                 lg = FALSE)

if (!binData & !medData) {
  modelResults <-
    if (RANDOM) {
      NMA_partial(
        winSource = "SurvWoodsREb.txt",
        modelParams = c("sd", "totresdev"))
    } else {
      NMA_partial(
        winSource = "SurvWoodsFEa.txt",
        modelParams = "totresdev")
    }
}

if (binData & !medData) {
  if (!RANDOM) {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsFEa_bin.txt"),
      dataFunc = setupData(
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
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  } else {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsREb_bin.txt"),
      dataFunc =
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
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  }
}

if (!binData & medData) {
  if (!RANDOM) {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsFEa_med.txt"),
      dataFunc = setupData(
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
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  } else {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsREb_med.txt"),
      dataFunc =
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
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  }
}

if (binData & medData) {
  if (!RANDOM) {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsFEa_med_bin.txt"),
      
      dataFunc = setupData(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin = subDataBin,
        is_random = RANDOM,
        refTx = REFTX),
      
      effectParam = "beta",
      modelParams = "totresdev",
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = FALSE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  } else {
    modelResults <- NMA(
      winSource = here::here("inst", "SurvWoodsREb_med_bin.txt"),
      dataFunc = setupData(
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
      random = TRUE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      decEff = decEff,
      lg = FALSE)
  }
}

#}

#########
# plots #
#########

plotNetwork(subData,
            subDataBin, binData,
            subDataMed, medData)

