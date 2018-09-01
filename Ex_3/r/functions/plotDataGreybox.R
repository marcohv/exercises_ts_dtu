plotDataGreybox <- function(X, rerun = FALSE)
    {
        ## Plot the time series (see "functions/plotTSBeg.R" to see the plot setup function)
        setpar("ts", mfrow=c(4,1))
        gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
        ## 
        #plot(X$timedate,X$yTi,type="n",xlab="",ylab="yTi ($^{\\circ}$C)",yaxt="n") # Prepare plot
        plot(X$timedate,X$yTi,type="n",xlab="",ylab=expression("yTi (" * degree * "C)"),yaxt="n") # Prepare plot
        axis(2,pretty(scalerange(X$yTi,0.2)))     # y axis
        abline(v=gridSeq,h=0,col="grey85",lty=3)  # Grid
        lines(X$timedate,X$yTiOr,col="grey60")
        lines(X$timedate,X$yTi)                   # draw lines
        ## 
        #plot(X$timedate,X$Te,type="n",xlab="",ylab="Te ($^{\\circ}$C)",yaxt="n")
        plot(X$timedate,X$Te,type="n",xlab="",ylab=expression("Te (" * degree * "C)"),yaxt="n")
        axis(2,pretty(scalerange(X$Te,0.2)))
        abline(v=gridSeq,h=0,col="grey85",lty=3)
        lines(X$timedate,X$Te)
        ## 
        plot(X$timedate,X$Qi,type="n",xlab="",ylab="Qi (kW)",yaxt="n")
        axis(2,pretty(scalerange(X$Qi,0.2)))
        abline(v=gridSeq,h=0,col="grey85",lty=3)
        lines(X$timedate,X$Qi)
        ## 
        #plot(X$timedate,X$Gv,type="n",xlab="",ylab="Gv (kW/m$^2$)",yaxt="n")
        plot(X$timedate,X$Gv,type="n",xlab="",ylab=expression("Gv (kW/" * m^2 * ")"),yaxt="n")
        axis(2,pretty(scalerange(X$Gv,0.2)))
        abline(v=gridSeq,h=0,col="grey85",lty=3)
        lines(X$timedate,X$Gv)
        ##
        plotTSXAxis(X$timedate,format="%Y-%m-%d")
    }
