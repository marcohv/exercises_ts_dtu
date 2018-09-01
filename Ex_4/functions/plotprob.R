plotprob <- function(Lfor, nm.out, main=NA, ylab="", ...){
  ## L is a list with data.frames containing prob. forecasts with columns named "q.." e.g. "q0.05"
  iprd <- grep("q.*$", names(Lfor[[1]]))
  ## Lfor is a list with data.frames containing forecasts
  setpar("ts")
  layout(cbind(1:length(Lfor),length(Lfor)+1),widths=c(1,0.075))
  ## For system
  for(i in 1:length(Lfor)){
    X <- Lfor[[i]]
    X$t <- X$t
    ## Plot the quantiles
    ##
    tmp <- X[ ,iprd]
    ##
    plot(X$t, X[,nm.out], pch=19, cex=0.5, type="n", ylim=range(tmp,X[ ,nm.out],na.rm=TRUE), ylab=ylab, ...)
    ##----------------------------------------------------------------
    ## Plot polygons
    nPolygons <- (ncol(tmp))/2
    ##
    color.gradient <- function(x, colors=c("red","bisque1"), colsteps=100) {
      return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
    }
    ##    colSeq <- colorJet(nPolygons)
    ##colSeq <- rev(hsv(seq(0.17,0,len=nPolygons), 1.0, 1.0))
    ##colSeq <- grey(seq(0.9,0.1,len=nPolygons))##colorJet(nPolygons)
    colSeq <- color.gradient(seq(0.9,0.1,len=nPolygons))
    ##
    for(j in 1:nPolygons)
    {
      ## Draw shaded region
      xpoly <- c(X$t, rev(X$t), X$t[1])
      ypoly <- c(tmp[,j], rev(tmp[,j+1]), c(tmp[1,j]))
      ypoly[is.na(ypoly)] <- 0
      polygon(xpoly, ypoly, col=colSeq[j], border=NA)
      ##
      ypoly <- c(tmp[,ncol(tmp)-j+1], rev(tmp[,ncol(tmp)-j]), c(tmp[1,ncol(tmp)-j+1]))
      ypoly[is.na(ypoly)] <- 0
      polygon(xpoly, ypoly, col=colSeq[j], border=NA)
    }
    ## Draw the median if unequal number of quantiles
    if(nPolygons%%1 != 0){ lines(X$t, tmp[ ,ceiling(nPolygons)], col=2, lwd=1) }
    ## Draw the observations
    points(X$t, X[ ,nm.out], pch=19, type="l", col=1, lwd=2)
    title(main=main[i], line=-1, cex.main=1.5)
  }
  axis.POSIXct(side=1, x=X$t, xaxt="s", format="%Y-%m-%d")
  mtext("Time", 1, line=1.5, cex=0.8)
  ## Legend
  ## Colorkey
  par(mar=c(4,1,4,1), cex.axis=0.8, mgp=c(1.2,0.15,0), tcl=-0.4)
  ##
  plot(0,type="n",yaxt="n",xlab="", xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")
  title(main="Quantiles", line=0.5)
  ##
  yval <- as.numeric(gsub("q","",names(X)[iprd]))#runPrm$quantiles
  nPolygons <- length(yval)
  yval <- c(yval,1)
  for(j in 1:(nPolygons-1))
  {
    ## Draw shaded region
    xpoly <- c(0,1,1,0,0)
    yvalbot <- yval[j]
    yvaltop <- yval[j+1]
    ypoly <- c(yvalbot,yvalbot,yvaltop,yvaltop,yvalbot)
    if((j-1) < (nPolygons-1)/2){
      colplot <- colSeq[j]
    }else{
      colplot <- colSeq[nPolygons-j]
    }
    polygon(xpoly, ypoly, col=colplot, border=NA)
  }
  ## Draw the median
  if(nPolygons%%2==1){lines(c(0,1),rep(yval[(nPolygons+1)/2],2), col=2)}
  ##
  axis(4,at=yval[-length(yval)],tcl=-0.3,las=1,mgp=c(2,0.5,0))
  ##
  invisible(X)
}
