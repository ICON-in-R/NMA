
#' Create BUGS code from component parts
#' 
create_bugs_code <- function(random, dat) {
  
  effect <- ifelse(random, "RE", "FE")
  med <- ifelse(any(is.na(dat$subDataMed)) | length(dat$subDataMed) == 0, NA, "med")
  bin <- ifelse(any(is.na(dat$subDataBin)) | length(dat$subDataBin) == 0, NA, "bin")

  data_code <- create_data_code(effect, med, bin)
  parameter_code <- create_parameter_code(effect, med, bin)
  model_code <- create_model_code(effect, med, bin)
  gq_code <- create_generated_quantities_code(effect, med, bin)
  
  script <- list()
  
  script$data <- paste0()
  script$parameters <- paste0()
  script$model <- paste0()
  script$generated_quantities <- paste0()
  
  # combine all elements into a complete BUGS model
  paste0(
    scode$data,
    scode$parameters,
    scode$model,
    scode$generated_quantities)
}

#
create_data_code <- function() {
  
}

#
create_parameter_code <- function() {
  
}

#
create_model_code <- function() {
  
}

#
create_generated_quantities_code <- function() {
  
}

