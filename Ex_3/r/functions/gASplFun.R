gASplFun <- function(fit,X,ylim=NA,addlines=FALSE,...)
  {
    nspl <- length(grep("^spl",names(X)))
    ## First the stationary response for the denominator
    B.2 <- sum(c(-1,fit$coefficients[grep("Qi",names(fit$coefficients))]))
    ## The gA as a function of the azimuth angle
    X$gAFun <- c(t(as.matrix(fit$coef[paste0("Gspl",1:nspl,".l0")])) %*% t(as.matrix(X[,paste0("spl",1:nspl)]))) / B.2
    ## Uncertainties
    i <- grep("^Gspl",names(fit$coef))
    spls <- t(as.matrix(X[,paste0("spl",1:nspl)]))
    ## For ARMAX the covar comes as a list
    if(class(fit)=="list") splsCov <- fit$covar[i,i]
    else splsCov <- vcov(fit)[i,i]
    ## First part is sum_{i=1}^{n} a_i^2 * Var(X_i)
    tmp <- diag(splsCov) %*% spls^2
    ## Second part is the covariance: 2 * ... see wikipedia
    tmp2 <- rep(NA,length(tmp))
    for(ii in 1:ncol(spls))
      {
        if(all(!is.na(spls[,ii])))
          {
            tmp2[ii] <- 2 * sum(spls[,ii]%*%t(spls[,ii]) * lower.tri(splsCov)*splsCov)
          }
      }
    X$gAFunSd <- c(tmp + tmp2)
    ## Take only one day, the second day
    X <- X[trunc(X$t,"day") == unique(trunc(X$t,"day"))[2], ]
    if(is.na(ylim[1])) ylim <- c(0,max(X$gAFun+2*sqrt(abs(X$gAFunSd)),na.rm=TRUE))#c(min(X$gAFun-2*sqrt(abs(X$gAFunSd)),na.rm=TRUE),max(X$gAFun+2*sqrt(abs(X$gAFunSd)),na.rm=TRUE))
    ##
    if(!addlines) plot(X$sunAzimuth,X$gAFun,type="l",ylab="gA splined",xlab="Sun azimuth",ylim=ylim,...)
    else lines(X$sunAzimuth,X$gAFun,...)
    ## A 95% confidence interval
    lines(X$sunAzimuth,X$gAFun-2*sqrt(abs(X$gAFunSd)),lty=2,...)
    lines(X$sunAzimuth,X$gAFun+2*sqrt(abs(X$gAFunSd)),lty=2,...)
  }
