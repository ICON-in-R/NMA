


codeDataMed <- function(subData, subDataMed, refTx) {
  
  txList <-
    unique(
      sort(
        c(subData$tx,
          subData$base,
          subDataMed$tx,
          subDataMed$base)))
  
  if (refTx %in% txList & !is.na(refTx)) {
    txList <-
      unique(c(refTx,
               sort(c(subData$tx,
                      subData$base,
                      subDataMed$tx,
                      subDataMed$base))))
  } else {
    refTx <- txList[1]
  }
  
  ##code treatments
  subData$Ltx <- codeVariable(var = subData$tx,
                               codeList = txList)
  
  subData$Lbase <-
    codeVariable(var = subData$base,
                 codeList = txList)
  
  subDataMed$mediantx <-
    codeVariable(var = subDataMed$tx,
                 codeList = txList)
  
  subDataMed$medianbase <-
    codeVariable(var = subDataMed$base,
                 codeList = txList)
  
  nTx <- length(txList)
  
  subData <- subData[order(subData$study, subData$tx),]
  
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx),]
  
  ## code studies
  studyList <- unique(sort(c(subData$study, subDataMed$study)))
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  subDataMed$medianstudy <-
    codeVariable(var = subDataMed$study, codeList = studyList)
  nStudies <<- max(c(subData$Lstudy, subDataMed$medianstudy) , 2)
  LnObs <<- length(subData$Lstudy)
  medianNObs <<- length(subDataMed$medianstudy)
  
  return(list(
    subData = subData,
    subDataMed = subDataMed,
    nTx = nTx,
    refTx = refTx,
    TxList = TxList))
}

