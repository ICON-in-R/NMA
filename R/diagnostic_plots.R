
#' WinBugs diagnostic plots
#' 
#' @param folder
#' @importFrom glue glue
#' 
#' @export
#' 
diagnostic_plots <- function(res_bugs,
                             labels,
                             folder = "output",
                             fileSep = "/") {
  
    createFolders(folder = "diagnostics", folder)
    
    curr_mcmc <- as.mcmc.list(res_bugs)
    
    # number of columns (variables)
    # [[1]] first element of mcmc list
    nc <- ncol(curr_mcmc[[1]])
    
    if (nc <= 30) {
      traceFileLocPNG <-
        glue("diagnostics{fileSep}{folder}{fileSep}trace_{labels$short}.png")
      png(
        filename = traceFileLocPNG,
        # start png (smaller file size than pdf)
        width = 4,
        height = 1.1 * nc,
        units = "in",
        # fix size in inches (except length grows with # variables)
        # Can play with this but will give errors if too large
        res = ifelse(nc <= 20, 300,          # resolution is 300 dpi unless too many variables
                     round(6000 / nc, -1))    # otherwise pick something lower
      )
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
      
      traceFileLocPNG1 <-
        paste("diagnostics", fileSep, folder, fileSep, "trace_", labels$short, "_part1.png", sep = "")
      traceFileLocPNG2 <-
        paste("diagnostics", fileSep, folder, fileSep, "trace_", labels$short, "_part2.png", sep = "")
      
      # same as above but plot one half at a time
      
      nc1 <- ceiling(nc / 2)
      nc2 <- nc - nc1
      
      # first half
      
      png(
        filename = traceFileLocPNG1,
        # start png (smaller file size than pdf) - PART 1!
        width = 4,
        height = 1.1 * nc1,
        units = "in",
        # fix size in inches (except length grows with # variables)
        # Can play with this but will give errors if too large.
        res = ifelse(nc1 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                     round(6000 / nc1, -1))   # otherwise pick something lower
      )
      
      # same number of rows as variables in mcmc object
      # a column each for trace and density
      par(mfrow = c(nc1, 2),
          mar = c(2, 2, 2, 1) + 0.1)              # small margins around each plot)
      
      for (ii in seq_len(nc1)) {
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
      
      # second half
      
      png(
        filename = traceFileLocPNG2,
        # start png (smaller file size than pdf) - PART 1!
        width = 4,
        height = 1.1 * nc2,
        units = "in",
        res = ifelse(nc2 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                     round(6000 / nc2, -1)))      # otherwise pick something lower
      
      par(mfrow = c(nc2, 2),
          mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
      
      for (ii in (nc1 + 1):nc) {
        # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
        traceplot(curr_mcmc[, ii])
        title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
        
        # density plot the same way. cex is scale on title font: 1 = no scale.
        densplot(curr_mcmc[, ii])
        title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep = ""), cex.main = 0.8)
      }
      dev.off()
      
    }
    
    traceFileLoc <-
      paste("diagnostics", fileSep, folder, fileSep, "trace_", labels$short, ".pdf", sep = "")

    pdf(file = traceFileLoc)
    plot(as.mcmc.list(res_bugs), ask = FALSE)
    dev.off()
    
    autocorrFileLoc <-
      paste("diagnostics", fileSep, folder, fileSep, "autocorr_", labels$short, ".pdf", sep = "")
    pdf(file = autocorrFileLoc)
    autocorr.plot(as.mcmc.list(res_bugs), ask = FALSE)
    dev.off()
    
    gelmanFileLoc <-
      paste("diagnostics", fileSep, folder, fileSep, "gelman_", labels$short, ".pdf", sep = "")
    pdf(file = gelmanFileLoc)
    gelman.plot(as.mcmc.list(res_bugs), ask = FALSE)
    dev.off()
  
  invisible(res_bugs)
}

