
codeData <- function(subData, refTx) {
  subData <<- subData
  
  txList <<- unique(sort(c(subData$tx, subData$base)))
  
  if (refTx %in% txList & !is.na(refTx)) {
    txList <<- unique(c(refTx, sort(c(
      subData$tx, subData$base
    ))))
  } else{
    refTx <<- txList[1]
  }
  
  ##code treatments
  subData$Ltx  <- codeVariable(var = subData$tx , codeList = txList)
  subData$Lbase  <-
    codeVariable(var = subData$base, codeList = txList)
  
  nTx <<- length(txList)
  subData <- subData[order(subData$study, subData$tx), ]
  
  ## code studies
  studyList <- unique(subData$study)
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  nStudies <<- max(subData$Lstudy , 2)
  nObs <<- length(subData$Lstudy)
  
  return(subData)
}


