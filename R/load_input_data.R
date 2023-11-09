
#' Load input data
#'
#' @param is_med Load median time data? logical
#' @param is_bin Load binary data? logical
#' @param analysis_type Appended to file name
#' @param folder Text string name
#'
#' @return
#' @export
#'
load_input_data <- function(is_med,
                            is_bin,
                            analysis_type,
                            folder = here::here("raw_data")) {

  
  file_name <- paste0(folder, "/survdata_", analysis$Endpoint, "_")
  
  subData <-
    read.csv(paste0(file_name, analysis_type, ".csv"),
             header = TRUE,
             as.is = TRUE)
  
  survDataBin <-
    if (is_bin) {
      read.csv(paste0(file_name, "bin.csv"),
               header = TRUE,
               as.is = TRUE)
    } else {NA}
  
  survDataMed <-
    if (is_med) {
      read.csv(paste0(file_name, "med.csv"),
               header = TRUE,
               as.is = TRUE) %>% 
        mutate(medR = floor(medR))
    } else {NA}  
  
  list(subData = subData,
       survDataMed = survDataMed,
       survDataBin = survDataBin)
}

