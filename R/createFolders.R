
#' Create output folders
#' 
#' @param folder Output folder. Text string
#' @param ... Additional arguments
#' 
#' @return 
#' @export
#' 
createFolders <- function(folder = "output",
                          ...) {
  sub_dir <- list(...)
  SYS <- .Platform$OS.type
  
  if (SYS == "windows") {
    if (!file.exists(folder))
      dir.create(path = here(folder))
    
    for (dir in sub_dir) {
      new_dir <- file.path(folder, dir)
      
      if (!file.exists(new_dir))
        dir.create(path = new_dir)
    }
  }
  
  if (SYS == "MAC") {
    if (!file.exists(folder))
      system(paste("mkdir", folder))
    
    for (dir in sub_dir) {
      dir_name <- file.path(folder, dir)
      
      if (!file.exists(dir_name))
        system(paste("mkdir", dir_name))
    }
  }
}

