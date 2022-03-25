
#' Save BUGS files
#'
#' @param res_bugs BUGS output 
#' @param bugs_params BUGS parameter names; strings
#' @param run_bugs Run BUGS? Logical
#' @param labels Labels
#' @param folder output folder name; string
#' @param overwrite Logical
#' @importFrom glue glue
#' 
#' @return res_bugs
#' @export
#'
save_bugs_files <- function(res_bugs,
                            bugs_params,
                            run_bugs,
                            labels,
                            folder = "output",
                            overwrite = TRUE) {
  
  SYS <- .Platform$OS.type
  
  if (class(res_bugs) != "try-error") {
    if (SYS == "windows" & run_bugs) {
      for (ii in seq_len(bugs_params$N.CHAINS)) {
        
        from_file <- paste0("inits", ii, ".txt")
        from_dir <- fs::path(tempdir(), from_file)
        
        to_file <- paste0(labels$short, "_", from_file)
        to_dir <- fs::path(folder, "model", to_file)
        
        fs::file_copy(path = from_dir, new_path = to_dir,
                      overwrite = overwrite)
      }
      
      to_file <- paste0(labels$short, "_data.txt")
      to_dir <- fs::path(folder, "model", to_file)
      
      fs::file_copy(path = fs::path(tempdir(), "data", ext = "txt"),
                    new_path = to_dir,
                    overwrite = overwrite)
    }
  }
  
  invisible(res_bugs)
}


