

#' WinnewBugs Function
#' 
genSamps <- function(bugsData,
                     n.iter = N.SIMS + N.BURNIN,
                     inits,
                     n.burnin = N.BURNIN,
                     newBugs.file,
                     parameters.to.save,
                     winDebug = PAUSE,
                     folder) {
  
  x <- newBugs(
    data = bugsData,
    parameters.to.save = parameters.to.save,
    model.file = newBugs.file,
    n.chains = N.CHAINS,
    inits = inits,
    n.iter = (N.SIMS * N.THIN) + N.BURNIN,
    n.burnin = N.BURNIN,
    n.thin = N.THIN
    #codaPkg=FALSE
    )
  
  if (PROG == "JAGS")
    x <- x$BUGSoutput
  
  if (DIAGNOSTICS) {
    #par(mar=c(4,4,4,4))
    
    #plot(as.mcmc.list(x),ask=FALSE)
    #savePlot(file=traceFileLoc,type="pdf")
    
    #autocorr.plot(as.mcmc.list(x),ask=FALSE)
    #savePlot(file=autocorrFileLoc,type="pdf")
    
    #gelman.plot(x,ask=FALSE)
    #savePlot(file=gelmanFileLoc,type="pdf")
    
    createFolders(folder = "diagnostics", folder)
    
    ##  ---------  start of new code for pngs  --------------------  ##
    #
    # This new code prints the traces/density plots to a single PNG,
    # with length (and resolution) dependent on the number of variables.
    #   Cons: can be REALLY long, and resolution may be poor
    #   Pros: small file size, almost no extra time to run, so can be
    #         run in addition to the pdfs.
    
    curr_mcmc <- as.mcmc.list(x)            # get list to be plotted
    
    # find number of columns (variables): [[1]] refers to the first element of the mcmc list
    nc <- ncol(curr_mcmc[[1]])
    
    if (nc <= 30) {
      traceFileLocPNG <<-
        paste("diagnostics",
              fileSep,
              folder,
              fileSep,
              "trace_",
              slabel,
              ".png",
              sep = "")
      # create the png name
      png(
        filename = traceFileLocPNG,
        # start png (smaller file size than pdf)
        width = 4,
        height = 1.1 * nc,
        units = "in",
        # fix size in inches (except length grows with # variables). Can play with this but will give errors if too large.
        res = ifelse(nc <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                     round(6000 / nc,-1))      # otherwise pick something lower
      )
      par(
        mfrow = c(nc, 2),
        # same number of rows as variables in mcmc object; a column each for trace and density
        mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
      for (ii in 1:nc) {
        # for each variable:
        
        # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
        traceplot(curr_mcmc[, ii])
        title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep =
                      ""), cex.main = 0.8)
        
        # density plot the same way. cex is scale on title font: 1 = no scale.
        densplot(curr_mcmc[, ii])
        title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep =
                      ""),
              cex.main = 0.8)
      }
      dev.off()
      
    } else {
      # if too many variables for a high resolution
      
      traceFileLocPNG1 <<-
        paste("diagnostics",
              fileSep,
              folder,
              fileSep,
              "trace_",
              slabel,
              "_part1.png",
              sep = "")
      traceFileLocPNG2 <<-
        paste("diagnostics",
              fileSep,
              folder,
              fileSep,
              "trace_",
              slabel,
              "_part2.png",
              sep = "")
      
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
        # fix size in inches (except length grows with # variables). Can play with this but will give errors if too large.
        res = ifelse(nc1 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                     round(6000 / nc1,-1))      # otherwise pick something lower
      )
      
      par(
        mfrow = c(nc1, 2),
        # same number of rows as variables in mcmc object; a column each for trace and density
        mar = c(2, 2, 2, 1) + 0.1)              # small margins around each plot)
      for (ii in 1:nc1) {
        # for each variable:
        
        # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
        traceplot(curr_mcmc[, ii])
        title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep =
                      ""), cex.main = 0.8)
        
        # density plot the same way. cex is scale on title font: 1 = no scale.
        densplot(curr_mcmc[, ii])
        title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep =
                      ""),
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
        # fix size in inches (except length grows with # variables). Can play with this but will give errors if too large.
        res = ifelse(nc2 <= 20, 300,          # resolution is 300 dpi unless too many variables for that
                     round(6000 / nc2,-1)))      # otherwise pick something lower
      
      par(
        mfrow = c(nc2, 2),
        # same number of rows as variables in mcmc object; a column each for trace and density
        mar = c(2, 2, 2, 1) + 0.1)               # small margins around each plot
      
      for (ii in (nc1 + 1):nc) {
        # for each variable:
        
        # trace plot. varnames gives e.g. 'baseMean', 'mu[5]'
        traceplot(curr_mcmc[, ii])
        title(paste("Trace plot for ", varnames(curr_mcmc)[ii], sep =
                      ""), cex.main = 0.8)
        
        # density plot the same way. cex is scale on title font: 1 = no scale.
        densplot(curr_mcmc[, ii])
        title(paste("Density plot for ", varnames(curr_mcmc)[ii], sep =
                      ""),
              cex.main = 0.8)
      }
      dev.off()
      
    }
    
    ##  ---------  end of new code for pngs  --------------------  ##
    
    traceFileLoc <<-
      paste("diagnostics",
            fileSep,
            folder,
            fileSep,
            "trace_",
            slabel,
            ".pdf",
            sep = "")
    pdf(file = traceFileLoc)
    plot(as.mcmc.list(x), ask = FALSE)
    dev.off()
    
    autocorrFileLoc <<-
      paste("diagnostics",
            fileSep,
            folder,
            fileSep,
            "autocorr_",
            slabel,
            ".pdf",
            sep = "")
    pdf(file = autocorrFileLoc)
    autocorr.plot(as.mcmc.list(x), ask = FALSE)
    dev.off()
    
    gelmanFileLoc <<-
      paste("diagnostics",
            fileSep,
            folder,
            fileSep,
            "gelman_",
            slabel,
            ".pdf",
            sep = "")
    pdf(file = gelmanFileLoc)
    gelman.plot(as.mcmc.list(x), ask = FALSE)
    dev.off()
    
    ##pdf(file=paste(folder,fileSep,"diagnostics",fileSep,"gelman_",label,".pdf",sep=""))
    #testGel <- try(gelman.plot(x,ask=FALSE))
    #    if (class(testGel)!="try-error") {newSavePlot(file=paste(folder,fileSep,"diagnostics",fileSep,"gelman_",slabel,".pdf",sep=""))}
    ##dev.off()
    
  }
  
  return(x)
  
}

