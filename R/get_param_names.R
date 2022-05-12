
#
get_param_names <- function(data_types = NA,
                            is_random = FALSE) {
  out <- 
    if ("bin_data" %in% data_types) {
      if (is_random) {
        c("d", "mu", "sd", "baseLod")
      } else {
        c("d", "mu", "baseLod")}
    } else if ("count_data" %in% data_types) {
      if (is_random) {
        c("mu", "d", "sd")
      } else {
        c("mu", "d")
      }
    } else if ("conts_data" %in% data_types) {
      if (is_random) {
        c("d", "mu", "sd", "B")
      } else {
        c("d", "mu", "B")}
    } else {
      if (is_random) {
        c("beta", "sd", "alpha")
      } else {
        c("beta", "alpha")}
    }
  
  out
}
