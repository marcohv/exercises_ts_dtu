## Plot all with prefix
plotseries <- function(X, prefix, gridTs, xnm, spaceForLegend=0.25, cex.legend=1, colmap=NA, ylim=NA, main="", ...)
{
##  browser()
  iseq <- integer(0)
  for(pf in strsplit(prefix, "\\|")[[1]]){
    iseq <- c(iseq,grep(pf,names(X)))
  }
  if(any(names(X)[iseq]==xnm)){
    iseq <- iseq[-which(names(X)[iseq]==xnm)]
  }
  ## Limits on y-axis
  if(is.na(ylim[1])){ ylim <- range(X[,iseq],na.rm=TRUE) }
  ##
  plot(X[ ,xnm],X[ ,xnm],type="n",xlim=c(min(X[ ,xnm]),max(X[ ,xnm])+diff(range(X[ ,xnm]))*spaceForLegend),ylim=ylim,yaxt="n",bty="n",xlab="",ylab="", ...)
  axis(2,pretty(scalerange(X[,iseq],0.2)))
  ## Grid
  if(!is.na(gridTs)) abline(v=seq(trunc(min(X[ ,xnm]),units="days"),max(X[ ,xnm]),by=gridTs), col="grey85")
  box()
  ##
  if(is.na(colmap[1])){ colmap <- 1:length(iseq) }
  ##
  for(i in 1:length(iseq))
  {
    lines( X[ ,xnm],  X[,iseq[i]], col=colmap[i], type="l", ...)
  }
  rng <- do.call("rbind",lapply(1:length(iseq), function(i){paste(format(range(X[,iseq[i]], na.rm=TRUE), digits=2), collapse=" to ")}))
  legend("topright", paste0(names(X)[iseq],": ",rng), lty=1, col=colmap, cex=cex.legend, ...)
  title(main=main, line=-1)
  invisible(iseq)
}

