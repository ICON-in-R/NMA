
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
  
  subData$Ltx <- match(subData$tx, txList)
  
  subData$Lbase <- match(subData$base, txList)
  
  subDataMed$mediantx <- match(subDataMed$tx, txList)
  
  subDataMed$medianbase  <- match(subDataMed$base, txList)
  
  subDataBin$Btx <- match(subDataBin$tx, txList)
  
  subDataBin$Bbase <- match(subDataBin$base, txList)
  
  nTx <- length(txList)
  
  subData <- subData[order(subData$study, subData$tx), ]
  
  subDataMed <-
    subDataMed[order(subDataMed$study, subDataMed$tx), ]
  
  subDataBin <-
    subDataBin[order(subDataBin$study, subDataBin$tx), ]
  
  studyList <-
    unique(c(subData$study, subDataMed$study, subDataBin$study))
  
  subData$Lstudy <- match(subData$study, studyList)
  
  subDataMed$medianstudy <- match(subDataMed$study, studyList)
  
  subDataBin$Bstudy <- match(subDataBin$study, studyList)
  
  nStudies <- max(c(subData$Lstudy, subDataMed$medianstudy), 2)
  
  LnObs <- length(subData$Lstudy)
  
  medianNObs <- length(subDataMed$medianstudy)
  
  BnObs <- length(subDataBin$Bstudy)
  
  return(list(
    subData = as.data.frame(subData),
    subDataMed = as.data.frame(subDataMed),
    subDataBin = as.data,frame(subDataBin),
    nStudies = nStudies,
    LnObs = LnObs,
    BnObs = BnObs,
    medianNObs = medianNObs,
    nTx = nTx,
    refTx = refTx,
    TxList = TxList))
}

