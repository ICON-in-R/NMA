
#' Make Report
#' 
#' Constructs the automated report from the output of the NMA.
#' 
#' @param ext A string of text to indicate the extension of the 
#' resulting output file. Possible options are \code{"pdf"}, \code{"docx"}.
#' This requires the use of pandoc, knitr and rmarkdown.
#' @param echo A string (default to \code{FALSE}) to instruct whether
#' the report should also include the \code{BCEA} commands used to 
#' produce the analyses. If the optional argument \code{echo} is set
#' to \code{TRUE} (default = \code{FALSE}), then the commands are also
#' printed.
#' @param ... Additional parameters.
#' 
#' @importFrom fs path file_copy
#' @importFrom withr with_options
#' @references
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' bugs_params <-
#' list(
#'   PROG = "openBugs",  # which version of BUGS to use to run the MCMC
#'   N.BURNIN = 10,#00,  # number of steps to throw away
#'   N.SIMS = 150,#0,    # total number of simulations
#'   N.CHAINS = 2,       # number of chains
#'   N.THIN = 1,         # thinning rate
#'   PAUSE = TRUE)
#' 
#' RANDOM <- FALSE             # is this a random effects model?
#' REFTX <- "X"                # reference treatment
#' data_type <- c("hr_data", "surv_bin_data", "med_data") # which type of data to use
#' label_name <- "label_name"
#' 
#' file_name <- here::here(file.path("inst", "extdata", "survdata_"))
#' 
#' survDataHR <-
#'   read.csv(paste0(file_name, "hr_test.csv"),
#'            header = TRUE,
#'            as.is = TRUE)
#' 
#' survDataBin <-
#'   tryCatch(
#'     read.csv(paste0(file_name, "bin_test.csv"),
#'              header = TRUE,
#'              as.is = TRUE),
#'     error = function(e) NA)
#' 
#' survDataMed <-
#'   tryCatch(
#'     read.csv(paste0(file_name, "med_test.csv"),
#'              header = TRUE,
#'              as.is = TRUE) %>% 
#'       mutate(medR = floor(medR)),
#'     error = function(e) NA)
#' 
#' nma_model <-
#'   new_NMA(survDataHR = survDataHR,
#'           survDataMed = survDataMed,
#'           survDataBin = survDataBin,
#'           bugs_params = bugs_params,
#'           is_random = RANDOM,
#'           data_type = data_type,
#'           refTx = REFTX ,
#'           effectParam = "beta",
#'           label = "",
#'           endpoint = "")
#' 
#' nma_model
#' 
#' make_report(nma_model, ext = "docx")
#' 
#' 
#' }
#' 
make_report <- function(nma_model,
                        ext = "pdf",
                        echo = FALSE,
                        ...) {
  extra_args <- list(...)
  
  filename <- 
    if (exists("filename", extra_args)) {
      extra_args$filename
    } else {
      paste0("Report.", ext)}
  
  show.tab <-
    if (exists("show.tab", extra_args)) {
      TRUE
    } else {
      FALSE}
  
  rmd_params <-
    list(nma_model = nma_model,
         filename = filename,
         show.tab = show.tab,
         ext = ext,
         echo = echo)
  
  # remove all warnings
  # local_options?
  withr::with_options(list(warn = -1), {
    
    # get current directory, move to relevant path,
    # go back to current directory
    file <- file.path(tempdir(), filename)
    
    file_location <-  
      normalizePath(
        fs::path(system.file("report", package = "NMA"), "report.Rmd"))
    
    rmd_format <-
      switch(ext,
             pdf = rmarkdown::pdf_document(),
             docx = rmarkdown::word_document())
    out <-
      quiet(
        rmarkdown::render(file_location,
                          output_format = rmd_format,
                          params = rmd_params))
    
    fs::file_copy(path = out, new_path = file, overwrite = TRUE)
    cat(paste0("The report is saved in the file ", file, "\n"))
  })
}

#' Allow disabling of the cat messages
#' @param x Object to quietly return
#' @keywords internal
#' 
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#' Automatically open pdf output using default pdf viewer
#' 
#' @param file_name String file names for pdf
#' @keywords internal
#' 
openPDF <- function(file_name) {
  os <- .Platform$OS.type
  if (os == "windows")
    shell.exec(normalizePath(file_name))
  else {
    pdf <- getOption("pdfviewer", default = '')
    if (nchar(pdf) == 0)
      stop("The 'pdfviewer' option is not set. Use options(pdfviewer = ...)")
    system2(pdf, args = c(file_name))
  }
}

