
#' Make BUGS model file name
#'
#' @param random Type of model, RE or FE; logical
#' @param data_type Data type
#'
#' @return string
#' @export
#'
make_bugs_filename <- function(random, data_type) {
  
  effect <- ifelse(random, "RE", "FE")
  data_name <- gsub(pattern = "_data", replacement = "", data_type)
  fname <- c(effect, data_name)
  
  file_name <- paste0(paste(fname, collapse = "_"), ".txt")
  fs::path_package(file_name, package = "NMA")
  # here::here("inst", file_name)
}

