
#' save_bugs_files
#'
#' @param res_bugs 
#' @param bugs_params 
#' @param run_bugs 
#' @param labels 
#' @param folder 
#' @param fileSep 
#' @importFrom glue glue
#' 
#' @return
#' @export
#'
save_bugs_files <- function(res_bugs,
                            bugs_params,
                            run_bugs,
                            labels,
                            folder = "output",
                            fileSep = "/") {
  
  SYS <- .Platform$OS.type
  
  if (class(res_bugs) != "try-error") {
    if (SYS == "windows" & run_bugs) {
      for (ii in seq_len(bugs_params$N.CHAINS)) {
        file.copy(from = glue("{tempdir()}{fileSep}inits{ii}.txt"),
                  to = here(glue("{folder}{fileSep}model{fileSep}{labels$short}_inits{ii}.txt")))
      }
      
      file.copy(from = glue("{tempdir()}{fileSep}data.txt"),
                to = here(glue("{folder}{fileSep}model{fileSep}{labels$short}_data.txt")))
    }
  }
  
  invisible(res_bugs)
}


