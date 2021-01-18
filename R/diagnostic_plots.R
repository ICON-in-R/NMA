
#' WinBugs diagnostic plots
#' 
#' @param folder
#' @importFrom glue glue
#' @importFrom here here
#' 
#' @export
#' 
diagnostic_plots <- function(res_bugs,
                             labels,
                             folder = "output",
                             fileSep = "/") {
  
  curr_mcmc <- as.mcmc.list(res_bugs)
  
  # number of columns (variables)
  nc <- ncol(curr_mcmc[[1]])
  
  if (nc <= 30) {
    trace_plot_name <-
      here(glue("{folder}{fileSep}diagnostics{fileSep}trace_{labels$short}.png"))
    png(
      filename = trace_plot_name,
      # start png (smaller file size than pdf)
      width = 4,
      height = 1.1 * nc,
      units = "in",
      # fix size in inches (except length grows with # variables)
      # Can play with this but will give errors if too large
      res = ifelse(nc <= 20,
                   yes = 300,                     # resolution 300 dpi unless too many variables
                   no = round(6000/nc, -1)))    # otherwise pick something lower
    
    # same number of rows as variables in mcmc object
    # a column each for trace and density
    par(mfrow = c(nc, 2),
        mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
    
    for (ii in seq_len(nc)) {
      # for each variable:
      
      # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
      traceplot(curr_mcmc[, ii])
      title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
      
      # density plot the same way. cex is scale on title font: 1 = no scale.
      densplot(curr_mcmc[, ii])
      title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep = ""),
            cex.main = 0.8)
    }
    dev.off()
    
  } else {
    # if too many variables for a high resolution
    
    trace_plot_name1 <-
      paste(folder, fileSep, "diagnostics", fileSep, "trace_", labels$short, "_part1.png", sep = "")
    trace_plot_name2 <-
      paste(folder, fileSep, "diagnostics", fileSep, "trace_", labels$short, "_part2.png", sep = "")
    
    # same as above but plot one half at a time
    nc1 <- ceiling(nc / 2)
    nc2 <- nc - nc1
    
    png(
      filename = trace_plot_name1,
      width = 4,
      height = 1.1 * nc1,
      units = "in",
      # fix size in inches (except length grows with # variables)
      # Can play with this but will give errors if too large.
      res = ifelse(nc1 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                   round(6000/nc1, -1))) # otherwise pick something lower
    
    # same number of rows as variables in mcmc object
    # a column each for trace and density
    par(mfrow = c(nc1, 2),
        mar = c(2, 2, 2, 1) + 0.1)              # small margins around each plot)
    
    for (ii in seq_len(nc1)) {
      # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
      traceplot(curr_mcmc[, ii])
      title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
      
      # density plot the same way. cex is scale on title font: 1 = no scale.
      densplot(curr_mcmc[, ii])
      title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep = ""),
            cex.main = 0.8)
    }
    dev.off()
    
    png(
      filename = trace_plot_name2,
      width = 4,
      height = 1.1 * nc2,
      units = "in",
      res = ifelse(nc2 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                   round(6000/nc2, -1)))    # otherwise pick something lower
    
    par(mfrow = c(nc2, 2),
        mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
    
    for (ii in (nc1 + 1):nc) {
      traceplot(curr_mcmc[, ii])
      title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
      
      # density plot the same way. cex is scale on title font: 1 = no scale.
      densplot(curr_mcmc[, ii])
      title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
    }
    dev.off()
    
  }
  
  traceFileLoc <-
    paste(folder, fileSep, "diagnostics", fileSep, "trace_", labels$short, ".pdf", sep = "")
  
  pdf(file = traceFileLoc)
  plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  autocorrFileLoc <-
    paste(folder, fileSep, "diagnostics", fileSep, "autocorr_", labels$short, ".pdf", sep = "")
  pdf(file = autocorrFileLoc)
  autocorr.plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  gelmanFileLoc <-
    paste(folder, fileSep, "diagnostics", fileSep, "gelman_", labels$short, ".pdf", sep = "")
  pdf(file = gelmanFileLoc)
  gelman.plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  invisible(res_bugs)
}

