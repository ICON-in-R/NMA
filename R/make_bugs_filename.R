
#' make_bugs_filename
#'
#' @param random logical
#' @param dat list
#'
#' @return string
#' @export
#'
make_bugs_filename <- function(random, dat) {
  
  effect <- ifelse(random, "RE", "FE")
  med <- ifelse(any(is.na(dat$subDataBin)), NA, "med")
  bin <- ifelse(any(is.na(dat$subDataBin)), NA, "bin")
  fname <- c(effect, med, bin)
  
  here("inst", paste0(paste(fname[!is.na(fname)], collapse = "_"), ".txt"))
}

