
#' Update NMA
#' 
#' Take an existing analysis and modify it.
#' 
#' @param nma Object of class \code{nma} 
#' @param ... Additional arguments
#'
#' @return
#' @export
#' @name NMA_update
#' @seealso \code{\link{new_NMA}}, \code{\link{NMA_run}}
#' 
NMA_update <- function(nma, ...) {
  UseMethod("NMA_update", nma)
}


#' @rdname NMA_update
#' 
#' @importFrom purrr map
#' @return
#' @export
#'
NMA_update.nma <- function(nma,
                           ...) {
  
  dots <- list(...)
  nma_call <- map(attributes(nma_model)$CALL, eval)
  params <- modifyList(nma_call, dots)
  
  do.call(new_NMA, params)
}

