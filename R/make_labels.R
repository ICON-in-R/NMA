
#' Make labels
#' @return list
#' 
make_labels <- function(label) {
  
  # Short label without spaces
  slabel <- sub(" ", "_", label)
  
  # Short label without spaces and FE/RE for data file
  sdlabel  <-
    sub(" ", "_", gsub("\\_RE", "", (gsub("\\_FE", "", label))))
  
  list(orig = label,
       short = slabel,
       refe = sdlabel)
}

