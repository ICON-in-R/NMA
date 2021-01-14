
#' prep_codeData
#'
#' @param subData 
#' @param subDataBin 
#' @param subDataMed 
#' @param refTx 
#'
#' @return
#' @export
#'
prep_codeData <- function(subData,
                          subDataBin = NA,
                          subDataMed = NA,
                          refTx) {
  txList <-
    unique(c(subData$tx,
             subData$base,
             subDataMed$tx,
             subDataMed$base,
             subDataBin$tx,
             subDataBin$base))
  
  if (refTx %in% txList & !is.na(refTx)) {
    txList <-
      unique(c(refTx,
               subData$tx,
               subData$base,
               subDataMed$tx,
               subDataMed$base,
               subDataBin$tx,
               subDataBin$base))
  } else {
    refTx <- txList[1]
  }
  
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
  
  nTx <- length(txList)
  
  subData <- subData[order(subData$study, subData$tx), ]
  
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx), ]
  
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx), ]
  
  studyList <-
    unique(c(subData$study, subDataMed$study, subDataBin$study))
  
  subData$Lstudy <-
    codeVariable(var = subData$study, codeList = studyList)
  
  subDataMed$medianstudy <-
    codeVariable(var = subDataMed$study, codeList = studyList)
  
  subDataBin$Bstudy <-
    codeVariable(var = subDataBin$study, codeList = studyList)
  
  nStudies <- max(c(subData$Lstudy, subDataMed$medianstudy), 2)
  
  LnObs <- length(subData$Lstudy)
  
  medianNObs <- length(subDataMed$medianstudy)
  
  BnObs <- length(subDataBin$Bstudy)
  
  subData <- as.data.frame(subDatalist$subData)
  subDataMed <- as.data.frame(subDatalist$subDataMed)
  subDataBin <- as.data.frame(subDatalist$subDataBin)

  return(list(
    subData = subData,
    subDataMed = subDataMed,
    subDataBin = subDataBin,
    nStudies = nStudies,
    LnObs = LnObs,
    BnObs = BnObs,
    medianNObs = medianNObs,
    nTx = nTx,
    refTx = refTx,
    TxList = TxList))
}

