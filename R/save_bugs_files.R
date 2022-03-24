
#' Save BUGS files
#'
#' @param res_bugs BUGS output 
#' @param bugs_params BUGS parameter names; strings
#' @param run_bugs Run BUGS? Logical
#' @param labels Labels
#' @param folder output folder name; string
#' @importFrom glue glue
#' 
#' @return res_bugs
#' @export
#'
save_bugs_files <- function(res_bugs,
                            bugs_params,
                            run_bugs,
                            labels,
                            folder = "output") {
  
  SYS <- .Platform$OS.type
  
  if (class(res_bugs) != "try-error") {
    if (SYS == "windows" & run_bugs) {
      for (ii in seq_len(bugs_params$N.CHAINS)) {
        
        from_file <- paste0("inits", ii, ".txt")
        from_dir <- file.path(tempdir(), from_file)
        
        to_file <- paste0(labels$short, "_inits", ii, ".txt")
        to_dir <- file.path(folder, "model", to_file)
        
        file.copy(from = from_dir, to = to_dir)
      }
      
      to_file <- paste0(labels$short, "_data.txt")
      to_dir <- file.path(folder, "model", to_file)
      
      file.copy(from = file.path(tempdir(), "data.txt"),
                to = to_dir)
    }
  }
  
  invisible(res_bugs)
}


