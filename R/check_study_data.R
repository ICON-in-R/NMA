
#
check_study_data <- function(subDataHR = NA,
                             subDataMed = NA,
                             subDataBin = NA,
                             countData  = NA,
                             contsData  = NA,
                             binData = NA) {
  
  main_col_names <- c("tx", "base", "study", "Lmean", "Lse", "multi_arm")
  main_missing_cols <- !all(main_col_names %in% names(subDataHR))
  
  if (main_missing_cols)
    stop("Main data set is missing columns", call. = FALSE)
  
  if (any(!is.na(subDataBin))) {
    bin_col_names <- c("tx", "base", "study", "BinN", "BinR")
    bin_missing_cols <- !all(bin_col_names %in% names(subDataBin))
    
    if (bin_missing_cols)
      stop("Binary data set is missing columns", call. = FALSE)
  }
  
  if (any(!is.na(subDataMed))) {
    med_col_names <- c("tx", "base", "study", "medN", "medR", "median")
    med_missing_cols <- !all(med_col_names %in% names(subDataMed))
    
    if (med_missing_cols)
      stop("Median data set is missing columns", call. = FALSE)
  }
}
