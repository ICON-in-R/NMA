
# N Green, UCL 7-12-2020 --------------------------------------------------

library(dplyr)


###### 4. Run NMA ######

## Settings

fileSep <- "\\"

RUN <- TRUE
#N.BURNIN <- 3000
#N.SIMS <- 15000

N.BURNIN <- 1000
N.SIMS <- 1500
N.CHAINS <- 2
N.THIN <- 1
PAUSE <- TRUE

PROG <- "openBugs"
DIAGNOSTICS <- TRUE

BASEPROB <- NA
decEff <- TRUE

# run analysis

# data
analysis <-
  read.csv(
    here::here("AnalysisList.csv"),
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

# selects indicator for avalibility of binary endpoint data
binDataInd <- currAnalysis$BinData

# selects indicator for avalibility of median endpoint data
medDataInd <- currAnalysis$MedData

binData <- binDataInd == "YES"
medData <- medDataInd == "YES"

#}


# label <- paste(label, "_", if (RANDOM == FALSE){"FE"} else {"RE"}, sep = "")

# Read in dataset
subData <-
  read.csv(
    paste(here::here(), "/survdata_", endpoint, "_", analysis_type, ".csv", sep = ""),
    header = TRUE,
    as.is = TRUE)

if (binData) {
  subDataBin <-
    read.csv(
      paste(here::here(), "/survdata_", endpoint, "_", "bin", ".csv", sep = ""),
      header = TRUE,
      as.is = TRUE)
}

if (medData) {
  subDataMed <-
    read.csv(
      paste(here::here(), "/survdata_", endpoint, "_", "med", ".csv", sep = ""),
      header = TRUE,
      as.is = TRUE)
}

if (binData == FALSE & medData == FALSE) {
  if (!RANDOM) {
    modelResults <- try(
      NMA(
        winSource = "SurvWoodsFEa.txt",
        dataFunc = setupData(
          subData = subData,
          subDataBin = subDataBin,
          subDataMed = subDataMed,
          random = RANDOM,
          refTx = REFTX,
          binData = binData,
          medData = medData),
        effectParam = c("beta"),
        modelParams = c("totresdev"),
        folder = endpoint,
        label = label,
        endpoint = endpoint,
        random = FALSE,
        binData = binData,
        medData = medData,
        refTx = REFTX,
        preRefTx = NA,
        decEff = decEff,
        lg = FALSE))
  } else {
    modelResults <- try(NMA(
      winSource = "SurvWoodsREb.txt",
      dataFunc = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = c("sd", "totresdev"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  }
}

if (binData == TRUE & medData == FALSE) {
  if (!RANDOM) {
    modelResults <- try(NMA(
      winSource = "SurvWoodsFEa_bin.txt",
      dataFunc = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,        
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = NA,
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = FALSE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  } else {
    modelResults <- try(NMA(
      winSource = "SurvWoodsREb_bin.txt",
      dataFunc = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,        
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = c("sd"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  }
}

if (binData == FALSE & medData == TRUE) {
  if (!RANDOM) {
    modelResults <- try(NMA(
      winSource = "SurvWoodsFEa_med.txt",
      dataFunc = setupData(
        subData = subData,
        subDataBin = subDataBin,
        subDataMed = subDataMed,
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = NA,
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = FALSE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  } else {
    modelResults <- try(NMA(
      winSource = "SurvWoodsREb_med.txt",
      dataFunc = setupData(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin = subDataBin,
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = c("sd"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  }
}

if (binData == TRUE & medData == TRUE) {
  if (!RANDOM) {
    modelResults <- try(NMA(
      winSource = "SurvWoodsFEa_med_bin.txt",
      dataFunc = setupData(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin = subDataBin,
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = c("totresdev"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = FALSE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  } else {
    modelResults <- try(NMA(
      winSource = "SurvWoodsREb_med_bin.txt",
      dataFunc = setupData(
        subData = subData,
        subDataMed = subDataMed,
        subDataBin = subDataBin,
        random = RANDOM,
        refTx = REFTX,
        binData = binData,
        medData = medData),
      effectParam = c("beta"),
      modelParams = c("sd", "totresdev"),
      folder = endpoint,
      label = label,
      endpoint = endpoint,
      random = TRUE,
      binData = binData,
      medData = medData,
      refTx = REFTX,
      preRefTx = NA,
      decEff = decEff,
      lg = FALSE))
  }
}

#}

#########
# plots #
#########

plotNetwork(subData,
            subDataBin, binData,
            subDataMed, medData)

