analyzeFit <- function(fit, tPer=NA, plotACF=TRUE, plotSeries=TRUE, plotMore=FALSE, newdev=FALSE,acfRmNA=FALSE,acftext=as.character(match.call())[2], ...)
    {
                                        #      browser()
        ## Take out the model,fit, and data from the list modelAndFit
        model <- fit$model
        Dat   <- fit$data[[1]]

        ## See the summary of the estimation result
        print(summary(fit,extended=TRUE))

        ##----------------------------------------------------------------
        ## Calculate the one-step predictions of the state (i.e. the residuals)
        tmp <- predict(fit)[[1]]

        ## Calculate the residuals and put them with the data in a data.frame X
        Dat$yTiHat <- tmp$output$pred$yTi
        Dat$residuals <- Dat$yTi - Dat$yTiHat
        Dat$TiSd <- tmp$state$sd$Ti
        Dat$yTiSd <- tmp$output$sd$yTi
        Dat$residualsStd <- Dat$residuals/Dat$yTiSd
        
        ## CTSM-R only keeps the data required for the fit. Get the timedate from X in the parent frame.
        Dat$timedate <- Dat$timedate

        ## Cut out a period if specified
        if(is.na(tPer[1])) 
            X <- Dat
        else
            X <- Dat[per(tPer[1],Dat$timedate,tPer[2]),]
        ##----------------------------------------------------------------
        
        if(plotACF)
            {
                res <- X$residuals/X$yTiSd
                
                ##----------------------------------------------------------------
                ## Plot the auto-correlation function and cumulated periodogram
                ## Open a new plot window?
                if(newdev) dev.new()
                ##
                par(mfrow=c(1,2),mar=c(3.2,3.2,3,1),mgp=c(2.2,0.7,0))
                ## The blue lines indicates the 95% confidence interval, meaning that if it is
                ##  white noise, then approximately 19 out of 20 lag correlations will be inside.
                if(acfRmNA) res <- res[!is.na(res)]
                ##
                acf(res, lag.max=8*24, main="")
                title(main="ACF of residuals",line=0.5)
                title(main=acftext,line=-2,cex.main=2)
                ## The periodogram is the estimated energy spectrum in the signal
                ##spec.pgram(res, main="Raw periodogram")
                ## The cumulated periodogram 
                cpgram(res,main="")
                title(main="Cumulated periodogram",line=0.5)
                ##----------------------------------------------------------------
            }

        if(plotSeries)
            {
                ##----------------------------------------------------------------
                ## Time series plots of the inputs and residuals
                ## A new plotting window?
                if(newdev) dev.new()
                ## Plot the time series (see "functions/plotTSBeg.R" to see the plot setup function)
                setpar("ts", mfrow=c(6,1))
                gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
                ## 
                #plot(X$timedate,X$residuals,ylab="$\\epsilon$ ~(\\txtdegree C)",type="n",xlab="",yaxt="n")
                plot(X$timedate,X$residuals,ylab=expression(epsilon~"("*degree*"C)"),type="n",xlab="",yaxt="n")
                axis(2,pretty(scalerange(X$residuals,0.2)))
                abline(v=gridSeq,h=0,col="grey92")
                lines(X$timedate,X$residuals,...)
                lines(X$timedate,X$yTiSd,col=4,...)
                lines(X$timedate,-X$yTiSd,col=4,...)
                ## Plot the standardized residuals
                #plot(X$timedate,X$residuals/X$yTiSd,ylab="$\\epsilon/\\sigma$",type="n",xlab="",yaxt="n")
                plot(X$timedate,X$residuals/X$yTiSd,ylab=expression(epsilon*"/"*sigma),type="n",xlab="",yaxt="n")
                axis(2,pretty(scalerange(X$residuals/X$yTiSd,0.2)))
                abline(v=gridSeq,h=0,col="grey92")
                lines(X$timedate,X$residuals/X$yTiSd,...)
                ##
                #plot(X$timedate,X$yTi,ylim=range(X[,c("yTi","yTiHat")],na.rm=TRUE),type="n",xlab="",ylab="$T\\n{i} ~(^{\\circ}$C)",yaxt="n")
                plot(X$timedate,X$yTi,ylim=range(X[,c("yTi","yTiHat")],na.rm=TRUE),type="n",xlab="",
                     ylab=expression(T[i]~"("*degree*"C)"),yaxt="n")
                axis(2,pretty(scalerange(X[,c("yTi","yTiHat")],0.2)))
                abline(v=gridSeq,h=0,col="grey85",lty=3)
                lines(X$timedate,X$yTi,...)
                lines(X$timedate,X$yTiHat,col=2,...)
                legend("topright",c("Measured","Predicted"),lty=1,col=1:2,bg="grey95",cex=0.8)
                ## 
                #plot(X$timedate,X$Qi,type="n",xlab="",ylab="$Q\\n{i}$ (W)",yaxt="n")
                plot(X$timedate,X$Qi,type="n",xlab="",ylab=expression(Q[i]~"(W)"),yaxt="n")
                axis(2,pretty(scalerange(X$Qi,0.2)))
                abline(v=gridSeq,h=0,col="grey85",lty=3)
                lines(X$timedate,X$Qi,...)
                ## 
                #plot(X$timedate,X$Gv,type="n",xlab="",ylab="$G\\n{v}$ (W/m$^2$)",yaxt="n")
                plot(X$timedate,X$Gv,type="n",xlab="",ylab=expression(G[v]~"(W/"*m^2*")"),yaxt="n")
                axis(2,pretty(scalerange(X$Gv,0.2)))
                abline(v=gridSeq,h=0,col="grey85",lty=3)
                lines(X$timedate,X$Gv,...)
                ## 
                #plot(X$timedate,X$Te,type="n",xlab="",ylab="$T\\n{e}$ ($^{\\circ}$C)",yaxt="n")
                plot(X$timedate,X$Te,type="n",xlab="",ylab=expression(T[e]~"("*degree*"C)"),yaxt="n")
                axis(2,pretty(scalerange(X$Te,0.2)))
                abline(v=gridSeq,h=0,col="grey85",lty=3)
                lines(X$timedate,X$Te,...)
                ##
                plotTSXAxis(X$timedate,format="%Y-%m-%d")
            }
        
        ##----------------------------------------------------------------
        ## Calculate the loglikelihood value
        print(paste("Loglikelihood", fit$loglik))
        ##----------------------------------------------------------------

        
        ##----------------------------------------------------------------
        ## The estimated HLC-value for the TiThTe model
        i <- which(names(fit$xm)%in%fit$Rnames)
        HLC <- 1/sum(fit$xm[i])
        print(HLC) ## W/C
        ## The covariance for the two estimated R values
        if(length(i)==1) cov <- fit$sd[i] * fit$corr[i,i] * fit$sd[i]
        else cov <- diag(fit$sd[i]) %*% fit$corr[i,i] %*% diag(fit$sd[i])
        ##
        print(paste("Cov = ",cov))

        ## Calculate the uncertainty of the HLC value with a linear approximation to the covariance
        ## The Jacobian, the derived of the HLC-value with respect to each estimate in fit$xm[i]
        J <- t( sapply(1:length(i), function(ii,x){ -1/sum(x)^2 }, x=fit$xm[i]) )
        ## The estimated variance of HLC
        varHLC <- J %*% cov %*% t(J)    
        ## and standard deviance
        sdHLC <- sqrt(varHLC)
        ## Return the confidence interval
        print(c(HLC-1.96*sdHLC,HLC+1.96*sdHLC))

        ## ## Calculate the uncertainty of the HLC value with a simulation approach
        ## ## Needed for multivariate normal distribution simulation
        ## require(MASS)
        ## ## Generate multivariate normal random values
        ## Rsim <- mvrnorm(n=1000000,mu=fit$xm[i],Sigma=cov)
        ## ## For each realization calculate the HLC-value
        ## HLCsim <- 1/apply(Rsim,1,sum)
        ## ## Estimate the 2.5% and 97.5% quantiles of the simulated values as a confidence interval
        ## quantile(HLCsim,probs=c(0.025,0.975))
        ## ##----------------------------------------------------------------

        if(plotMore)
            {
                ##----------------------------------------------------------------
                ## Time series plots residuals and more statistics
                ## A new plotting window?
                if(newdev) dev.new()
                ## Plot the time series (see "functions/plotTSBeg.R" to see the plot setup function)
                plotTSBeg(4)
                gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
                ## Plot the standardized residuals
                plot(X$timedate,X$residuals/X$yTiSd,ylab="Standardized residuals",type="n",xlab="",yaxt="n")
                axis(2,pretty(scalerange(X$residuals/X$yTiSd,0.2)))
                abline(v=gridSeq,h=0,col="grey92")
                lines(X$timedate,X$residuals/X$yTiSd,...)
                ##

                  # c er hubers psi
                psiLim <- model$options$hubersPsiLimit
#                ressq <- X$residuals^2 / X$yTiSd^2
#                ressq[which(ressq>psiLim^2)]<-psiLim*(2*sqrt(ressq[which(ressq>psiLim^2)])-psiLim)
                #(nll<- -( 0.5 * sum(log(X$residuals^2)+ressq,na.rm=TRUE) + 0.5 * sum(!is.na(X$residuals))*log(2*pi) ))

                ## ressq<-X$residuals^2/X$yTiSd^2
                ## ressq[which(ressq>psiLim^2)]<-psiLim*(2*sqrt(ressq[which(ressq>psiLim^2)])-psiLim)
                ## plot(ressq)
                ## (nll<--sum(log(X$yTiSd^2)+ressq))
                ##browser()
                print(fit$loglik)
  
                ressq <- X$residuals^2/X$yTiSd^2
                ressq[which(ressq>psiLim^2)]<-psiLim*(2*sqrt(ressq[which(ressq>psiLim^2)])-psiLim)
                print(nll <-  0.5*sum(log(X$yTiSd^2)+ressq,na.rm=TRUE) + 0.5*sum(!is.na(X$residuals))*log(2*pi) )

                ## The 
                X$loglik <- 0.5*(log(X$yTiSd^2)+ressq)+0.5*log(2*pi)*!is.na(X$residuals)
                plot(X$timedate,X$loglik,ylab="Likelihood",type="n",xlab="",yaxt="n")
                axis(2,pretty(scalerange(X$loglik,0.2)))
                abline(v=gridSeq,h=0,col="grey92")
                lines(X$timedate,X$loglik,...)
                ##
                #plot(X$timedate,X$residuals,ylab="Residuals ($^{\\circ}$C)",type="n",xlab="",yaxt="n")
                plot(X$timedate,X$residuals,ylab=expression("Residuals ("*degree*"C)"),type="n",xlab="",yaxt="n")
                axis(2,pretty(scalerange(X$residuals,0.2)))
                abline(v=gridSeq,h=0,col="grey92")
                lines(X$timedate,X$residuals,...)
                lines(X$timedate,X$yTiSd,col=4,...)
                lines(X$timedate,-X$yTiSd,col=4,...)
                lines(X$timedate,X$TiSd,col=2,...)
                lines(X$timedate,-X$TiSd,col=2,...)
                ##
                #plot(X$timedate,X$yTi,ylim=range(X[,c("yTi","yTiHat")],na.rm=TRUE),type="n",xlab="",ylab="yTi, yTiHat ($^{\\circ}$C)",yaxt="n")
                plot(X$timedate,X$yTi,ylim=range(X[,c("yTi","yTiHat")],na.rm=TRUE),type="n",xlab="",
                     ylab=expression("yTi, yTiHat ("*degree*"C)"),yaxt="n")
                axis(2,pretty(scalerange(X[,c("yTi","yTiHat")],0.2)))
                abline(v=gridSeq,h=0,col="grey85",lty=3)
                lines(X$timedate,X$yTi,...)
                lines(X$timedate,X$yTiHat,col=2,...)
                legend("bottomright",c("Measured","Predicted"),lty=1,col=1:2,bg="grey95")
            }

        ## 
        invisible(Dat$residualsStd)
    }

