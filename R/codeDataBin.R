
codeDataBin <- function(subData, subDataBin, refTx) {
  subData <<- subData
  subDataBin <<- subDataBin
  
  #subData$tx <- sub('[[:space:]]+$', '',subData$tx )
  
  txList <<-
    unique(sort(c(
      subData$tx, subData$base, subDataBin$tx, subDataBin$base
    )))
  
  if ((refTx %in% txList) == TRUE & (!is.na(refTx)) == TRUE) {
    txList <<-
      unique(c(refTx, sort(
        c(subData$tx, subData$base, subDataBin$tx, subDataBin$base)
      )))
  } else{
    refTx <<- txList[1]
  }
  
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx, codeList = txList)
  subData$Lbase <-
    codeVariable(var = subData$base, codeList = txList)
  subDataBin$Btx <-
    codeVariable(var = subDataBin$tx, codeList = txList)
  subDataBin$Bbase <-
    codeVariable(var = subDataBin$base, codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx),]
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx),]
  
  ## code studies
  studyList <- unique(sort(c(subData$study, subDataBin$study)))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataBin$Bstudy <-
    codeVariable(var = subDataBin$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataBin$Bstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  BnObs <<- length(subDataBin$Bstudy)
  
  return(list(subData, subDataBin))
}

