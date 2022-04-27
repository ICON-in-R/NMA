
#' Load all input data from a folder
#'
#' @param data_dir folder location; string
#' @return \code{nma} object
#' @importFrom fs path
#' 
#' @export
#' 
new_NMA_dir <- function(data_dir = ".") {
  
  reference <- read.csv(fs::path(data_dir, "REFERENCE", ext = "csv"))
  
  analysis_filename <- reference$file[reference$type == "analysis"]
  bugs_filename <- reference$file[reference$type == "bugs"]
  subData_filename <- reference$file[reference$type == "subData"]
  subDataBin_filename <- reference$file[reference$type == "subDataBin"]
  subDataMed_filename <- reference$file[reference$type == "subDataMed"]
  
  subData <-
    read.csv(fs::path(data_dir, subData_filename),
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
  
  subDataBin <-
    if (length(subDataBin_filename) != 0) {
      read.csv(fs::path(data_dir, subDataBin_filename),
               stringsAsFactors = FALSE,
               header = TRUE,
               as.is = TRUE)
    } else {NA}
  
  subDataMed <-
    if (length(subDataMed_filename) != 0) {
      read.csv(fs::path(data_dir, subDataMed_filename),
               stringsAsFactors = FALSE,
               header = TRUE,
               as.is = TRUE) %>% 
        mutate(medR = floor(medR))
    } else {NA}
  
  NMA_params <- 
    c(analysis_params,
      list(subData = subData,
           subDataBin = subDataBin,
           subDataMed = subDataMed,
           bugs_params = bugs_params))
  
  do.call(new_NMA, NMA_params)
}

