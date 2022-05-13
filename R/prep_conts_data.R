
#' prep_conts_data
#'
#' @param contsData 
#' @param refTx
#' @importFrom sqldf sqldf
#' @importFrom plyr ddply
#'
#' @return
#' @export
#'
prep_conts_data <- function(contsData, refTx = NA, covars = NA) {

  if(all(!is.na(covars))) {
    
    studyMean <-
      plyr::ddply(subData, .(study),
                  function(x) data.frame(Study_covar = weighted.mean(x$covars, x$n)))
    contsData <- merge(contsData, studyMean, by = "study")
    
  }
  
  armCount <- table(contsData$study)
  oneArmStudies <- names(armCount[armCount == 1])
  contsData <- contsData[!contsData$study %in% oneArmStudies, ]
  
  contsData$meanSE <- paste0(round(contsData$y, digits = 2),
                             "(", round(contsData$se, digits = 2), ")")   
  
  contsData <- removeDup(studyData = contsData,
                         studyName = "study",
                         treatName = "treatment",
                         studyDur_min = studyDur_min,
                         studyDur_max = studyDur_max,
                         durName = "dur")
  
  ## codeData()
  
  txList <- unique(sort(contsData$treatment))
  
  contsData$prop <- paste(contsData$r, "\\", contsData$E, sep = " ")   
  
  if(refTx %in% txList && !is.na(refTx)) {
    txList <- unique(c(refTx, sort(contsData$treatment)))
  } else {
    refTx <- txList[1]
  }
  
  contsData$tx <- match(contsData$treatment, table = txList)
  nTx <- length(txList)
  
  for (i in seq_len(nrow(contsData))) {
    tx_in_s <- contsData[contsData$study == contsData$study[i], ]$tx
    contsData$numTx[i] <- length(unique(tx_in_s))
  }
  
  contsData <- contsData[order(contsData$numTx, contsData$study, contsData$tx), ]
  
  # base treatment for each study
  tx_lup <- contsData |> 
    transmute(base_treatment = treatment,
              baseTx = tx) |> 
    unique()
  
  contsData <- 
    contsData |> 
    group_by(study) |> 
    mutate(base_treatment = min(treatment)) |> 
    inner_join(tx_lup, by = "base_treatment") 
  
  # code studies
  studyList <- unique(contsData$study)
  contsData$studyCode <- match(contsData$study, table = studyList)
  nStudies <- max(contsData$studyCode, 2)
  nObs <- length(contsData$studyCode)
  
  ###
  
  
  long_data <- reshape2::melt(contsData, id.vars = c("study","treatment"))
  
  y <- reshape2::dcast(long_data[long_data$variable == "meanSE", ],
                       study ~ treatment)
  
  wide_data <- NULL
  s_names_rev <- rev(unique(contsData$study))
  
  for (i in s_names_rev) {
    wide_data <- rbind(wide_data, y[y$study == i, ])
  }
  
  mx <- mean(contsData$covars, na.rm = TRUE)
  
  maxArms <- max(table(contsData$study))
  tAna <- rep(NA, nStudies)
  tAt <- matrix(NA, nStudies, maxArms)  
  tAy <- matrix(NA, nStudies, maxArms)
  tAse <- matrix(NA, nStudies, maxArms)
  if (!is.na(covars)) tAx <- rep(NA, nStudies)
  
  for (i in seq_len(nStudies)) {
    tAna[i] <- length(contsData$studyCode[contsData$studyCode == i])    
    tAy[i,1:tAna[i]] <- contsData$y[contsData$studyCode == i]
    tAse[i,1:tAna[i]] <- contsData$se[contsData$studyCode == i]
    tAt[i,1:tAna[i]] <- contsData$tx[contsData$studyCode == i]
    
    if (!is.na(covars)) {
      if (covType == "Cont") {
        tAx[i] <- contsData$Study_covar[contsData$studyCode == i]
      } else {
        tAx[i] <- contsData$covars[contsData$studyCode == i]
      }
    }
  }
  
  list(t = tAt,
       y = tAy,
       se = tAse,
       nt = nTx,
       na = tAna,
       x = tAx,
       ns = nStudies,
       mx = mx)
}


##TODO: remove this
removeDup <- function(studyData,
                      studyName,
                      treatName,
                      studyDur_min,
                      studyDur_max,
                      durName) {
  
  if (studyDur_max == 9999) {
    # if duration max for analysis is 9999 then target is min
    targetDur <- studyDur_min
  } else {
    # else target duration is midpoint of range
    targetDur <- round(median(c(studyDur_min,studyDur_max)))
  } 
  
  # Remove duplicates - keep the data closest to the target duration
  # Calculate absolute residual between target and observed duration
  studyData[,"resTargetDur"] <- abs(targetDur - studyData[,durName])
  # Sort on residual
  studyData_sort <- studyData[order(studyData$resTargetDur,decreasing = FALSE), ]
  
  # Identify unique studies with time point nearest to target
  studyData_uniqueStudies <-
    studyData_sort[!duplicated(studyData_sort[,c(studyName,treatName)],fromLast = FALSE), ]
  
  # Where arms have been given the same name within a study#
  # keep these as long as criteria above is fulfilled
  # Identify studies with multiple treatment arm
  studyData_multTx <-
    studyData_sort[duplicated(studyData_sort[,c(studyName,treatName,durName)],fromLast = FALSE), ]
  
  # Create indicator for matches between the two datasets
  match_ind <-
    as.data.frame(studyData_multTx[,studyName] %in% studyData_uniqueStudies[,studyName] &
                    studyData_multTx[,treatName] %in% studyData_uniqueStudies[,treatName] &
                    studyData_multTx[,durName] %in% studyData_uniqueStudies[,durName])  
  
  colnames(match_ind) <- "Duplicate"
  
  if (nrow(match_ind) != 0) {
    # Bind indicator column with multiple arm dataset
    match_arm <- subset(cbind(studyData_multTx,match_ind),Duplicate == TRUE)
    match_arm$Duplicate <- NULL  # Remove duplicate indicator
    # Append unique studies dataset with matched arm dataset
    studyData <- rbind(studyData_uniqueStudies,match_arm)
  } else {
    studyData <- studyData_uniqueStudies
  }
  
  studyData
}

