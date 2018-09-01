analyzeFit <- function(fit,tPer = NA, newdev=TRUE, plotit=TRUE)
  {
    ## Keep the default plotting parameters for resetting the plotting device below
    def.par <- par(no.readonly=TRUE)
    ## See the summary of the estimation
    print(summary(fit))
    print(summary(fit,extended=TRUE))

    ##----------------------------------------------------------------
    ## Take the data
    tmp <- fit$data[[1]]
    ## Calculate the one-step predictions of the state (i.e. the residuals)
    tmp$yTiHat <- predict(fit)[[1]]$output$pred$yTi
    ## Calculate the residuals and them in the data
    tmp$residuals <- tmp$yTi - tmp$yTiHat

    ## Cut out a period is specified
    if(!is.na(tPer[1])) tmp <- tmp[per(tPer[1],tmp$timedate,tPer[2]),]

    if(plotit)
    {
      ## Plot the auto-correlation function and cumulated periodogram in a new window
      if(newdev) dev.new()
      par(mfrow=c(1,3))
      ## The blue lines indicates the 95% confidence interval, meaning that if it is
      ##  white noise, then approximately 19 out of 20 lag correlations will be inside.
      acf(tmp$residuals, lag.max=8*24)
      title(main=as.character(match.call())[2],line=-2,cex.main=2)
      ## The periodogram is the estimated energy spectrum in the signal
      spec.pgram(tmp$residuals)
      ## The cumulated periodogram 
      cpgram(tmp$residuals)
      ## Set the plotting parameters to default again
      par(def.par)
      ##----------------------------------------------------------------
  
  
      ##----------------------------------------------------------------
      ## Time series plots of the inputs and residuals
      ## A new plotting window
      if(newdev) x11()
      ## Plot the residuals
      setpar("ts",mfrow=c(5,1))
      gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
      ##
      plot(tmp$timedate,tmp$residuals,type="n",ylab="residuals")
      abline(v=gridSeq,h=0,col="grey92")
      lines(tmp$timedate,tmp$residuals)
      title(main=as.character(match.call())[2],line=-2,cex.main=2)
      ## 
      plot(tmp$timedate,tmp$Ph,type="n",ylab="Ph")
      abline(v=gridSeq,h=0,col="grey92")
      lines(tmp$timedate,tmp$Ph)
      ## 
      plot(tmp$timedate,tmp$yTi,type="n",ylab="yTi")
      abline(v=gridSeq,h=0,col="grey92")
      lines(tmp$timedate,tmp$yTi)
      lines(tmp$timedate,tmp$yTiHat,col=2)
      legend("bottomright",c("Measured","Predicted"),lty=1,col=1:2,bg="grey95")
      ## 
      plot(tmp$timedate,tmp$Ta,type="n",ylab="Ta")
      abline(v=gridSeq,h=0,col="grey92")
      lines(tmp$timedate,tmp$Ta)
      ##
      plot(tmp$timedate,tmp$Ps,type="n",ylab="Ps")
      abline(v=gridSeq,h=0,col="grey92")
      lines(tmp$timedate,tmp$Ps)
      ##
      plotTSXAxis(tmp$timedate,format="%Y-%m-%d")
      ## Set the plotting parameters to default again
      par(def.par)
      layout(1)
      ##----------------------------------------------------------------
    }
    ## Return the residuals
    invisible(tmp$residuals)
}
