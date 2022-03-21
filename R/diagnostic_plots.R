
#' WinBUGS diagnostic plots
#' 
#' @param res_bugs Output from a BUGS run
#' @param labels $short is used in file name save
#' @param folder Text string name
#' @param fileSep  File separator; default forward slash
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom coda as.mcmc.list traceplot varnames densplot autocorr.plot gelman.plot
#' 
#' @export
#' 
diagnostic_plots <- function(res_bugs,
                             labels_short = "",
                             folder = "output") {
  
  curr_mcmc <- as.mcmc.list(res_bugs)
  
  # number of columns (variables)
  nc <- ncol(curr_mcmc[[1]])
  
  if(nc <= 30) {
    file_name <- paste0("trace_", labels_short, ".png")
    trace_plot_name <-
      file.path(folder, "diagnostics", file_name)
    
    png(
      filename = trace_plot_name,
      # start png (smaller file size than pdf)
      width = 4,
      height = 1.1 * nc,
      units = "in",
      # fix size in inches (except length grows with # variables)
      # Can play with this but will give errors if too large
      res = ifelse(nc <= 20,
                   yes = 300,                   # resolution 300 dpi unless too many variables
                   no = round(6000/nc, -1)))    # otherwise pick something lower
    
    # same number of rows as variables in mcmc object
    # a column each for trace and density
    par(mfrow = c(nc, 2),
        mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
    
    for (ii in seq_len(nc)) {
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
    
    file_name1 <- paste0("trace_", labels_short, "_part1.png")
    trace_plot_name1 <-
      file.path(folder, "diagnostics", file_name1)

    file_name2 <- paste0("trace_", labels_short, "_part2.png")
    trace_plot_name2 <-
      file.path(folder, "diagnostics", file_name2)
    
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
      res = ifelse(nc2 <= 20, 300,
                   round(6000/nc2, -1)))
    
    par(mfrow = c(nc2, 2),
        mar = c(2, 2, 2, 1) + 0.1)
    
    for (ii in (nc1 + 1):nc) {
      traceplot(curr_mcmc[, ii])
      title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
      
      densplot(curr_mcmc[, ii])
      title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
    }
    dev.off()
  }
  
  # trace
  
  file_name <- paste0("trace_", labels_short, ".pdf")
  traceFileLoc <-
    file.path(folder, "diagnostics", file_name)
  
  pdf(file = traceFileLoc)
  plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  # autocorrelation
  
  file_name <- paste0("autocorr_", labels_short, ".pdf")
  autocorrFileLoc <-
    file.path(folder, "diagnostics", file_name)
  
  pdf(file = autocorrFileLoc)
  autocorr.plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  # gelman-rubin
  
  file_name <- paste0("gelman_", labels_short, ".pdf")
  gelmanFileLoc <-
    file.path(folder, "diagnostics", file_name)
  
  pdf(file = gelmanFileLoc)
  gelman.plot(as.mcmc.list(res_bugs), ask = FALSE)
  dev.off()
  
  invisible(res_bugs)
}

