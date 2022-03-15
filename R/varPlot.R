##TODO: what is this?

#' Study network diagram
#' 
#' @param xVar x variable
#' @param yVar y variable
#' @param showYLab logical
#' @param showXLab logical
#' @param pointText point text
#' @param joinY join y
#' @param txList List of treatments
#' @param folder Folder name; string
#' @param label label
#' @param cex.axis size axis
#' @param ... Additional arguments
#' 
varPlot <- function(xVar,
                    yVar ,
                    showYLab = TRUE,
                    showXLab = TRUE,
                    pointText = NA,
                    joinY = TRUE ,
                    txList = NA,
                    folder,
                    label,
                    cex.axis = 0.2,
                    ...) {
  
  xVar <- as.character(xVar)
  #y <- as.character(y)
  numX <- as.numeric(factor(xVar))
  numY <- as.numeric(factor(yVar, levels = unique(yVar)))
  
  if (!is.na(txList[1]))
    numX <- match(xVar, txList)
  if (is.na(txList[1]))
    txList <- unique(xVar)
  
  byVar <- TRUE
  
  plot(
    numX,
    numY,
    type = "n",
    xlab = " ",
    ylab = " ",
    bty = "n",
    yaxt = "n",
    xaxt = "n")
  
  for (currY in unique(yVar)) {
    #  points(x=numX[yVar==currY],y=numY[yVar==currY],cex=0.2,col="black",pch=19)
    # text(x=numX[yVar==currY],y=numY[yVar==currY],labels=subData$r[yVar==currY],cex=0.7)
    if (joinY)
      lines(
        x = numX[yVar == currY],
        y = numY[yVar == currY],
        cex = 0.35,
        col = "grey",
        pch = 19,
        lwd = 3)
  }
  
  abline(v = seq(along = unique(xVar)), col = "light grey")
  
  for (currY in unique(yVar)) {
    if (is.na(pointText)) {
      points(
        x = numX[yVar == currY],
        y = numY[yVar == currY],
        cex = 0.8,
        col = "black",
        pch = 19)
    } else {
      text(
        x = numX[yVar == currY],
        y = numY[yVar == currY],
        labels = pointText[yVar == currY],
        cex = cex.axis)
    }
  }
  
  if (showXLab)
    axis(
      1,
      at = 1:max(numX),
      lab = txList,
      las = 2,
      lwd = 0.35,
      cex.axis = 0.5,
      cex = 2,
      cex.lab = 2)
  if (showYLab)
    axis(
      2,
      at = 1:max(numY),
      lab = levels(factor(yVar, levels = unique(yVar))),
      lwd = 0.35,
      las = 1,
      cex.axis = 0.5,
      cex = 0.5,
      cex.lab = 0.5)
  
  mtext(
    text = label,
    side = 3,
    cex = 0.8,
    padj = -1)
}

