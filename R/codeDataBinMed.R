
#'
codeDataBinMed <- function(subData, subDataBin, subDataMed, refTx) {
  subData <<- subData
  subDataMed <<- subDataMed
  subDataBin <<- subDataBin
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<-
    unique(sort(
      c(
        subData$tx,
        subData$base,
        subDataMed$tx,
        subDataMed$base,
        subDataBin$tx,
        subDataBin$base
      )
    ))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<-
      unique(c(refTx, sort(
        c(
          subData$tx,
          subData$base,
          subDataMed$tx,
          subDataMed$base,
          subDataBin$tx,
          subDataBin$base
        )
      )))
  } else{
    refTx <<- txList[1]
  }
  
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx , codeList = txList)
  subData$Lbase  <-
    codeVariable(var = subData$base , codeList = txList)
  subDataMed$mediantx  <-
    codeVariable(var = subDataMed$tx , codeList = txList)
  subDataMed$medianbase  <-
    codeVariable(var = subDataMed$base , codeList = txList)
  subDataBin$Btx  <-
    codeVariable(var = subDataBin$tx , codeList = txList)
  subDataBin$Bbase  <-
    codeVariable(var = subDataBin$base , codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx),]
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx),]
  
  ## code studies
  studyList <-
    unique(sort(c(
      subData$study, subDataMed$study, subDataBin$study
    )))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataMed$medianstudy <-
    codeVariable(var = subDataMed$study, codeList = studyList)
  subDataBin$Bstudy <-
    codeVariable(var = subDataBin$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataMed$medianstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  medianNObs <<- length(subDataMed$medianstudy)
  BnObs <<- length(subDataBin$Bstudy)
  
  return(list(subData, subDataMed, subDataBin))
}

