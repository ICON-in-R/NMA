
#' Create output folders
#' 
#' @param folder Text string name
#' @param fileSep File separate; default forward slash
#' @param ... Additional arguments
#' 
#' @importFrom glue glue
#' 
#' @return 
#' @export
#' 
createFolders <- function(folder = "output",
                          fileSep = "/",
                          ...) {
  
  sub_dir <- list(...)
  
  SYS <- .Platform$OS.type
  
  if (SYS == "windows") {
    if (!file.exists(folder))
      dir.create(path = here(folder))
    
    for (dir in sub_dir) {
      new_dir <- here(glue("{folder}{fileSep}{dir}"))
      
      if (!file.exists(new_dir))
        dir.create(path = new_dir)
    }
  }
  
  if (SYS == "MAC") {
    if (!file.exists(folder))
      system(paste("mkdir ", folder, sep = ""))
    for (dir in sub_dir) {
      if (!file.exists(paste(folder, fileSep, dir, sep = "")))
        system(paste("mkdir ", folder, fileSep, dir, sep = ""))
    }
  }
}

