
#'
file_manip <- function() {
  
  SYS <- .Platform$OS.type
  
  # Short label without spaces
  slabel <- sub(" ", "_", label)
  
  # Short label without spaces and FE/RE for data file
  sdlabel  <-
    sub(" ", "_", gsub("\\_RE", "", (gsub("\\_FE", "", label))))
  
  createFolders(folder = folder, "results", "graphs", "model", "sims", "data")
  
  if (class(res_bugs) != "try-error") {
    if (SYS == "windows" & RUN) {
      for (ii in seq_len(N.CHAINS)) {
        ##TODO~ use R command instead...
        system(paste(
          Sys.getenv("COMSPEC"), "/c",
          glue("copy {tempdir()}{fileSep} inits {ii}.txt {folder}{fileSep}model{fileSep}{slabel}_inits{ii}.txt")
        ))
      }
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0("copy ", tempdir(), fileSep, "data.txt ", folder, fileSep, "model", fileSep, slabel, "_data.txt")
      ))
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste0("copy ", tempdir(), fileSep, winSource, " ", folder, fileSep, "model", fileSep, slabel, "_", winSource)
      ))
    }
    
    save(file = paste0(folder, fileSep, "model", fileSep, "bugsObject_", slabel), res_bugs)
  }
}


