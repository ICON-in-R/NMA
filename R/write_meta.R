
#' Analysis Meta-Data
#' @importFrom yaml write_yaml
#' 
write_meta <- function(nma, folder = "output") {
  
  metadata <- c(
    date = as.character(Sys.time()),
    Rversion = R.version.string,
    nma[c("is_med", "is_bin", "bugs_params", "is_random",
          "refTx", "effectParam", "modelParams", "endpoint")])
  
  yaml::write_yaml(
    metadata,
    file = fs::path(folder, "metadata", ext = "yaml"))
}
