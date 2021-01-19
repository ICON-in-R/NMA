
#' create output folders
#' 
createFolders <- function(folder,
                          fileSep = "/",
                          ...) {
  
  subFolders <- list(...)
  
  ##TODO: why not just use inbuilt R commands?
  
  SYS <- .Platform$OS.type
  
  if (SYS == "windows") {
    if (!file.exists(folder))
      system(paste(
        Sys.getenv("COMSPEC"),
        "/c",
        paste("mkdir ", folder, sep = "")
      ))
    for (subFolder in subFolders) {
      if (!file.exists(paste(folder, fileSep, subFolder, sep = "")))
        system(paste(
          Sys.getenv("COMSPEC"),
          "/c",
          paste("mkdir ", folder, fileSep, subFolder, sep = "")
        ))
    }
  }
  
  if (SYS == "MAC") {
    if (!file.exists(folder))
      system(paste("mkdir ", folder, sep = ""))
    for (subFolder in subFolders) {
      if (!file.exists(paste(folder, fileSep, subFolder, sep = "")))
        system(paste("mkdir ", folder, fileSep, subFolder, sep = ""))
    }
  }
  
}

