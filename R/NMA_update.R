
#' Update NMA
#'
#' Take an existing NMA analysis created with an `nma` object, modify the input
#' arguments and rerun it.
#'
#' @template args-nma
#' @param ... Additional arguments
#'
#' @return
#' @export
#' @name NMA_update
#' @seealso `new_NMA()`, `NMA_run()`
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

