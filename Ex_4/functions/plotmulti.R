## Plot all with prefix
plotmulti <- function(X,prefixes=NULL,xnm="t",gridTs=NA,xlabs=NA,ylabs=NA, spaceForLegend=0, cex.legend=1, format="%Y-%m-%d %H:%M", colmap=NA, ylim=NA, main="", ...)
{
  if(is.null(prefixes)){ prefixes <- names(X) }
##   browser()
  prefixes <- prefixes[prefixes!=xnm]
  oldpar <- setpar("ts",mfrow=c(length(prefixes),1))
  on.exit(par(oldpar))
  for(i in 1:length(prefixes))
  {
    if(is.na(ylim[1])){ ylim <- NA }else{ ylim <- ylim[[i]]}
    if(is.na(colmap[1])){ colmap <- NA }else{ colmap <- colmap[[i]]}
    ##
    plotseries(X, prefixes[i], gridTs, xnm, spaceForLegend=spaceForLegend, cex.legend=cex.legend, colmap, ylim, main[i], ...)
        if(!is.na(ylabs[1])) title(ylab=ylabs[i], yaxt="s")
      }
  if(xnm%in%c("t","timedate")){
    if(class(X[ ,xnm])[1] == "numeric"){ axis(1, X[ ,xnm], xaxt="s") }
    else{
      axis.POSIXct(1, X[ ,xnm], format=format, xaxt="s")
    }
  }
}
## ## Plot all with prefix
## plotmultigg <- function(X,prefixes,gridTs=NA,xlabs=NA,ylabs=NA,...)
## {
##       ## Plot
##     require("ggplot2")
##     require("reshape2")
##     require("gridExtra")
    
##     ## Multiple with same y-axis
##   ## Get the data to plot


##   length(prefixes)

##     for(i in 1:length(prefixes))
##     {
##       ## Melt the data to plot, with "t" and the columns
##       tmp <- melt(X[ ,c("t",grep(prefixes[i],names(X)))], "t")
  


  
##     X <- Xplot()
##     ## Only the selected boxes
##     X <- X[X$id %in% selectedBoxIds(), ]
##     ##
##     Lplot <- list()
##     i <- 0
##     for(var in input$vars){
##       ##
##       tmp <- X[X$variable==var, ]
##       if(nrow(tmp)>0){
##         ## By color
##         i <- i + 1
##         Lplot[[i]] <- ggplot(tmp, aes(x=t, y=value, color=id)) + labs(title=var) + geom_line()
##       }
##     }
##     ml <- marrangeGrob(Lplot, nrow=5, ncol=1, top="")
##     print(ml)


  
##     setpar("ts",mfrow=c(length(prefixes),1))
##     for(i in 1:length(prefixes))
##       {
##         plotseries(X,prefixes[i],gridTs,...)
##         if(!is.na(ylabs[1])) title(ylab=ylabs[i], yaxt="s")
##       }
##     axis.POSIXct(1, X$t,format=c("%m-%d %H:%M"), xaxt="s")
##   }
