
#
check_study_data <- function(survDataHR = NA,
                             survDataMed = NA,
                             survDataBin = NA,
                             countData  = NA,
                             contsData  = NA,
                             binData = NA) {
  
  if (any(!is.na(survDataHR))) {
    hr_col_names <- c("tx", "base", "study", "Lmean", "Lse", "multi_arm")
    hr_missing_cols <- !all(hr_col_names %in% names(survDataHR))
    
    if (hr_missing_cols)
      stop("HR data set is missing columns", call. = FALSE)
  }
  
  if (any(!is.na(survDataBin))) {
    bin_col_names <- c("tx", "base", "study", "BinN", "BinR")
    bin_missing_cols <- !all(bin_col_names %in% names(survDataBin))
    
    if (bin_missing_cols)
      stop("Binary data set is missing columns", call. = FALSE)
  }
  
  if (any(!is.na(survDataMed))) {
    med_col_names <- c("tx", "base", "study", "medN", "medR", "median")
    med_missing_cols <- !all(med_col_names %in% names(survDataMed))
    
    if (med_missing_cols)
      stop("Median data set is missing columns", call. = FALSE)
  }
}
