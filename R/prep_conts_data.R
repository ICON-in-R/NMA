
#
prep_conts_data <- function(contsData) {
  
  studyMean <- ddply(contsData, .(study),
                     function(x) data.frame(Study_covar = weighted.mean(x$covars,x$n)))
  
  contsData <- merge(contsData,studyMean, by = "study")
  
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
  ##TODO: what is this doing? remove SQL
  idBase <- function(idVar, byVar) {
    idData <- data.frame(idVar = idVar, byVar = byVar) 
    baseVar <-
      sqldf("select baseVar from idData as a inner join (select byVar, min(idVar)
          as baseVar from idData group by byVar) as b on a.byVar=b.byVar")
    return(unlist(baseVar))
  }
  
  txList <- unique(sort(binData$treatment))
  
  subData$prop <- paste(subData$r,"\\",subData$E, sep=" ")   
  
  if(refTx %in% txList && !is.na(refTx)) {
    txList <- unique(c(refTx, sort(binData$treatment)))
  } else {
    refTx <- txList[1]
  }
  
  binData$tx <- match(binData$treatment, table = txList)
  nTx <- length(txList)
  
  for (i in seq_len(nrow(binData))) {
    tx_in_s <- binData[binData$study == binData$study[i], ]$tx
    binData$numTx[i] <- length(unique(tx_in_s))
  }
  
  binData <- binData[order(binData$numTx, binData$study, binData$tx), ]
  
  # identify baseline treatment
  ##TODO: remove function
  binData$baseTx <- idBase(idVar = binData$tx, byVar = binData$study)
  
  # code studies
  studyList <- unique(binData$study)
  binData$studyCode <- match(binData$study, table = studyList)
  nStudies <- max(binData$studyCode, 2)
  nObs <- length(binData$studyCode)
  
  ###
  
  
  long_data <- reshape2::melt(contsData, id.vars = c("study","treatment"))
  y <- reshape2::dcast(long_data[long_data$variable == "meanSE", ], study ~ treatment)
  
  wide_data <- NULL
  s_names_rev <- rev(unique(binData$study))
  
  for (i in s_names_rev) {
    wide_data <- rbind(wide_data, y[y$study == i, ])
  }
  
  mx <- mean(subData$covars, na.rm = TRUE)
  
  maxArms <- max(table(contsData$study))
  tAna <- rep(NA, nStudies)
  tAt <- matrix(NA, nStudies, maxArms)  
  tAy <- matrix(NA, nStudies, maxArms)
  tAse <- matrix(NA, nStudies, maxArms)
  if (!is.na(covars)) tAx <- rep(NA, nStudies)
  
  for (i in seq_len(nStudies)) {
    tAna[i] <- length(contsData$studyCode[subData$studyCode == i])    
    tAy[i,1:tAna[i]] <- contsData$y[subData$studyCode == i]
    tAse[i,1:tAna[i]] <- contsData$se[subData$studyCode == i]
    tAt[i,1:tAna[i]] <- contsData$tx[subData$studyCode == i]
    
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

