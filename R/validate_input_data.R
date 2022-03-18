##TODO: remove duplication

#
validate_bugs_param <- function(params) {
  
  properties <-
    list(PROG = list(type = as.character, c("openbugs", "winbugs")),
         N.BURNIN = list(type = as.integer, NA),
         N.SIMS = list(type = as.integer, NA),
         N.CHAINS = list(type = as.integer, NA),
         N.THIN = list(type = as.integer, NA),
         PAUSE = list(type = as.logical, NA))
  
  param_names <-
    names(properties)[names(properties) %in% names(params)]
  
  for (i in param_names) {
    as_type <- properties[[i]]$type
    params[[i]] <- as_type(trimws(params[[i]]))
  }
  params
}


#
validate_analysis_param <- function(params) {
  
  properties <-
    list(is_random = list(type = as.logical, NA),
         refTx = list(type = as.character, NA),
         effectParam = list(type = as.character, NA),
         modelParams = list(type = as.character, NA),
         label = list(type = as.character, NA),
         endpoint = list(type = as.character, NA))
  
  param_names <-
    names(properties)[names(properties) %in% names(params)]
  
  for (i in param_names) {
    as_type <- properties[[i]]$type
    params[[i]] <- as_type(trimws(params[[i]]))
  }
  params
}

