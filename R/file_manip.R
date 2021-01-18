
#'
file_manip <- function(labels,
                       folder = "output",
                       fileSep = "/") {
  
  SYS <- .Platform$OS.type
  
  createFolders(folder = folder, "results", "graphs", "model", "sims", "data")
  
  if (class(res_bugs) != "try-error") {
    if (SYS == "windows" & RUN) {
      for (ii in seq_len(N.CHAINS)) {
        ##TODO~ use R command instead...
        system(paste(
          Sys.getenv("COMSPEC"), "/c",
          glue("copy {tempdir()}{fileSep} inits {ii}.txt {folder}{fileSep}model{fileSep}{labels$short}_inits{ii}.txt")
        ))
      }
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0("copy ", tempdir(), fileSep, "data.txt ", folder, fileSep, "model", fileSep, labels$short, "_data.txt")
      ))
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0("copy ", tempdir(), fileSep, winSource, " ", folder, fileSep, "model", fileSep, labels$short, "_", winSource)
      ))
    }
  }
  
  return()
}


