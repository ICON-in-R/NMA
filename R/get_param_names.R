
#
get_param_names <- function(is_bin,
                            is_count,
                            is_conts,
                            is_random) {
  out <- 
    if (is_bin) {
      if (is_random) {
        c("d", "mu", "sd", "baseLod")
      } else {
        c("d", "mu", "baseLod")}
    } else if (is_count) {
      if (is_random) {
        c("mu", "d", "sd")
      } else {
        c("mu", "d")
      }
    } else if (is_conts) {
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
