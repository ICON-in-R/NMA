
#' Write study data to file
#' 
#' @template args-nma
#' @param label string
#' @param folder Name of folder; string
#' @param ... Additional arguments
#' @export
#' 
write_data_to_file <- function(nma,
                               label = "",
                               folder = "output",
                               ...) {
  UseMethod("write_data_to_file", nma)
}


#' @rdname write_data_to_file
#' @export
#' 
write_data_to_file.nma <- function(nma,
                                   label = "",
                                   folder = "output",
                                   ...) {
  dat <- nma$dat
  
  file_name <- paste0("data_", label, ".csv")
  dir_name <- file.path(folder, "data", file_name)
  
  writeLines(
    paste0(
      "Key: Lmean=mean log hazard ratios;\n
            Lse=standard error for log hazard ratios;\n
            multi=multi-arm trial indicator\n",
      "------------------"), dir_name)
  
  suppressWarnings(
    write.table(
      dat$subData,
      file = dir_name,
      sep = ",",
      append = TRUE,
      col.names = NA))
  
  if (all(!is.na(dat$survDataBin))) {
    
    file_name <- paste0("data_", label, "_bin.csv")
    dir_nameBin <- file.path(folder, "data", file_name)
    
    writeLines(
      paste0(
        "Key: Bn=Number of patients in arm;\n
              Br=number of events in arm\n",
        "------------------"),
      dir_nameBin)
    
    suppressWarnings(
      write.table(
        dat$survDataBin,
        file = dir_nameBin,
        sep = ",",
        append = TRUE,
        col.names = NA))
  }
  
  if (all(!is.na(dat$survDataMed))) {
    
    file_name <- paste0("data_", label, "_med.csv")
    dir_nameMed <- file.path(folder, "data", file_name)
    
    writeLines(
      paste0("Key: ...\n",
             "------------------\n"),
      dir_nameMed)
    
    suppressWarnings(
      write.table(
        dat$survDataMed,
        file = dir_nameMed,
        sep = ",",
        append = TRUE,
        col.names = NA))
  }
}

