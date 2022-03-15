
#' Make BUGS filename
#'
#' @param random Type of model, RE or FE; logical
#' @param dat List of data frames. Possible from subData/Med/Bin
#'
#' @return string
#' @export
#'
make_bugs_filename <- function(random, dat) {
  
  effect <- ifelse(random, "RE", "FE")
  med <- ifelse(any(is.na(dat$subDataBin)), NA, "med")  ##TODO: should this be subDataMed?
  bin <- ifelse(any(is.na(dat$subDataBin)), NA, "bin")
  fname <- c(effect, med, bin)
  
  here("inst", paste0(paste(fname[!is.na(fname)], collapse = "_"), ".txt"))
}

