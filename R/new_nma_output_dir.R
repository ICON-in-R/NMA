
#' Create output folders
#' 
#' @template args-nma
#' @param folder Output folder. Text string
#' @param ... Additional arguments
#' @import fs
#' 
#' @return 
#' @seealso new_NMA
#' @export
#' 
new_nma_output_dir <- function(nma,
                               folder = "output",
                               ...) {
  
  ellipsis::check_dots_unnamed()
  
  if (fs::file_exists(folder)) {
    message(strwrap("Will over-write previous output folder.
        If you don't want to do this change the folder argument.",
        prefix = " ", initial = ""))
  }
  
  sub_dir_names <-
    c("data", "diagnostics", "graphs", "model", "results", "sims")
  sub_dir <- c(sub_dir_names, list(...))
  SYS <- .Platform$OS.type
  
  if (SYS == "windows") {
    if (!fs::file_exists(folder))
      fs::dir_create(path = here(folder))
    
    for (dir in sub_dir) {
      new_dir <- fs::path(folder, dir)
      
      if (!fs::file_exists(new_dir))
        fs::dir_create(path = new_dir)
    }
  }
  
  if (SYS == "MAC") {
    if (!fs::file_exists(folder))
      system(paste("mkdir", folder))
    
    for (dir in sub_dir) {
      dir_name <- fs::path(folder, dir)
      
      if (!fs::file_exists(dir_name))
        system(paste("mkdir", dir_name))
    }
  }
  
  write_meta(nma, folder) 
}

