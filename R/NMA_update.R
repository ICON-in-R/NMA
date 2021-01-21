
NMA_update <- function(nma) {
  UseMethod("NMA_update", nma)
}


NMA_update.nma <- function(nma,
                           ...) {
  
  dots <- list()
  
  params <- modifyList(nma$.call, dots)
  
  do.call(new_NMA, params)
}

