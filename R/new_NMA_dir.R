
#' Load all input data from a folder
#'
#' Rather than script the prepocessing, read in data from a folder containing a
#' `REFERENCE` file telling R what and where the different input data file are.
#' 
#' @param data_dir folder location; string
#' @return `nma` object
#' @importFrom fs path
#'
#' @export
#' 
new_NMA_dir <- function(data_dir = ".") {
  
  reference <- read.csv(fs::path(data_dir, "REFERENCE", ext = "csv"))
  
  analysis_filename <- reference$file[reference$type == "analysis"]
  bugs_filename <- reference$file[reference$type == "bugs"]
  survDataHR_filename <- reference$file[reference$type == "subData"]
  survDataBin_filename <- reference$file[reference$type == "survDataBin"]
  survDataMed_filename <- reference$file[reference$type == "survDataMed"]
  
  subData <-
    read.csv(fs::path(data_dir, survDataHR_filename),
             stringsAsFactors = FALSE,
             header = TRUE,
             as.is = TRUE)
  
  bugs_params <-
    read.csv(fs::path(data_dir, bugs_filename),
             stringsAsFactors = FALSE,
             header = FALSE) %>% 
    keyval_as_list %>% 
    validate_bugs_param
  
  analysis_params <-
    read.csv(fs::path(data_dir, analysis_filename),
             stringsAsFactors = FALSE,
             header = FALSE) %>% 
    keyval_as_list %>% 
    validate_analysis_param
  
  survDataBin <-
    if (length(survDataBin_filename) != 0) {
      read.csv(fs::path(data_dir, survDataBin_filename),
               stringsAsFactors = FALSE,
               header = TRUE,
               as.is = TRUE)
    } else {NA}
  
  survDataMed <-
    if (length(survDataMed_filename) != 0) {
      read.csv(fs::path(data_dir, survDataMed_filename),
               stringsAsFactors = FALSE,
               header = TRUE,
               as.is = TRUE) %>% 
        mutate(medR = floor(medR))
    } else {NA}
  
  NMA_params <- 
    c(analysis_params,
      list(subData = subData,
           survDataBin = survDataBin,
           survDataMed = survDataMed,
           bugs_params = bugs_params))
  
  do.call(new_NMA, NMA_params)
}

