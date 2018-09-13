##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Packages and functions used
require(splines)
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")
##----------------------------------------------------------------


##----------------------------------------------------------------
## RLS for load forecasting

## Make a data.frame with synced observations and NWPs
k <- 24
X <- data.frame(t = Data$t, Ph = Data$Ph4, Ta = lag_vector(Data$Tanwp[ ,pst("k",k)],k), G = lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday = Data$tday)

## Make a training set, first 3 month, and a test set
## Just keep the indexes
itrain <- which(per("2010-09-01",X$t,"2011-02-01"))
itest <- which(per("2011-02-01",X$t,"2011-04-01"))

## Cut only the nessecary period
X <- X[per("2010-09-01",X$t,"2011-04-01"), ]

## Fit with RLS
fit <- rls(Ph ~ bs(tday,df=3) + Ta + G, lambda = 0.99, data = X, k = k)

## Predictions
X$yhat <- fit$y - fit$residuals

## Plot them for a period
i <- per("2010-10-01",X$t,"2010-11-01")
plot(X$t[i], X$Ph[i], type = "l")
lines(X$t[i], X$yhat[i], type = "l", col = 2)

## The tracked coefficients
plot.ts(fit$Theta[i, ])
##----------------------------------------------------------------


##----------------------------------------------------------------
## Check the first period
i <- per("2010-09-01",X$t,"2010-10-01")
plot(X$t[i], X$Ph[i], type = "l")
lines(X$t[i], X$yhat[i], type = "l", col = 2)

## The tracked coefficients
plot.ts(fit$Theta[i, ])
##----------------------------------------------------------------


##----------------------------------------------------------------
## Define an objective function for tuning the prm (i.e. lp coefficients and lambda)

## Extend the objective function to have a "burn-in" period (the score is only evaluated on indexes in ieval)
obj <- function(prm, frml, data, k, return_fit = FALSE, ieval = 1:nrow(data)) {
    print(prm)
    ## Apply a low-pass filter on the input
    data$Ta <- lp_vector(data$Ta, a1 = prm[1])
    data$G <- lp_vector(data$G, a1 = prm[2])
    lambda <- prm[3]
    fit <- rls(as.formula(frml), lambda, data, k)
    ## Evaluate only on the ieval rows
    print(score <- rmse(fit$residuals[ieval]))
    if(return_fit){
        return(fit)
    }else{
        return(score)
    }
}

## Set ieval (here don't evaluate on the first 14 days
ieval <- itrain[-1:-(24*14)]

## Test objective function
frml <- "Ph ~ bs(tday,df=3) + Ta + G"
obj(c(0.95,0.95,0.99), frml, X[itrain, ], k = k, ieval = ieval)

## Tune the prms
result <- optim(c(0.95,0.95,0.99), obj, lower = c(0.3,0.3,0.9), upper = c(0.999,0.999,0.999), frml = frml, data = X[itrain, ], k = k, ieval = ieval, method = "L-BFGS-B")

## Return the fit
fit <- obj(result$par, frml, X, k = k, return_fit = TRUE)

## Predictions
X$Ph_hat_rls <- fit$y - fit$residuals

## Plot the test set 
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_rls[itest], col = 2)

## The score on the test set
rmse(X$Ph[itest] - X$Ph_hat_rls[itest])

## See how the coefficients changed over time
## Plot for the test set
plot(X$t[itest], X$Ph[itest], ylim=range(fit$Theta[itest, ],na.rm=TRUE), type = "n")
lines(X$t[itest], fit$Theta[itest,1], col = 1)
lines(X$t[itest], fit$Theta[itest,2], col = 2)
lines(X$t[itest], fit$Theta[itest,3], col = 3)
##----------------------------------------------------------------



##----------------------------------------------------------------
## (Beyond scope) Calculate the forecasts for multiple horizons
## Can take some time to run, but included to give the script calculating the full forecasts

## In order to use all cores on the cpu
library(parallel)

## How many cores does this machine have
(ncores <- detectCores())

## Do it for horizons 1 to 4
kseq <- 1:4

## Which house?
ihouse <- 2

## Do it parallel using all cores
L <- mclapply(kseq, function(k){
    X <- data.frame(t = Data$t, Ph = Data[[pst("Ph",ihouse)]], Ta = lag_vector(Data$Tanwp[ ,pst("k",k)],k), G = lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday = Data$tday)
    ##
    result <- optim(c(0.95,0.95,0.99), obj, lower = c(0.3,0.3,0.9), upper = c(0.999,0.999,0.999), formula=frml, data=X[itrain, ], k = k, ieval = ieval, method = "L-BFGS-B")
    ##
    fit <- obj(result$par, frml, X, k = k, return_fit = TRUE)
    ##
    list(fit = fit, par = result$par)
}, mc.cores = ncores)

## See the parameters
L[[1]]$par
L[[2]]$par
L[[3]]$par
## Or in one line using sapply
sapply(L, function(x){ return(x$par) })

## Could put together the forecasts to get them for each point in time
Data$Ph_hat <- as.data.frame(sapply(L, function(x){
  x$fit$y - x$fit$residuals
}))
names(Data$Ph_hat) <- pst("k",kseq)

## Compare the k = 1 and k = 2 forecasts
plot(Data$t[itest], Data[[pst("Ph",ihouse)]][itest], type="l")
lines(Data$t[itest], Data$Ph_hat$k1[itest], col=2)
lines(Data$t[itest], Data$Ph_hat$k2[itest], col=3)

## Do it for horizons 1 to 36, just using the optimized parameters from k = 4
kseq <- 1:36
tmp <- mclapply(kseq, function(k){
  X <- data.frame(t = Data$t, Ph = Data[[pst("Ph",ihouse)]], Ta = lag_vector(Data$Tanwp[ ,pst("k",k)],k), G = lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday = Data$tday)
  fit <- obj(L[[4]]$par, frml, X, k = k, return_fit = TRUE)
  list(fit = fit, par = result$par)
})
## Could put together the forecasts to get them for each point in time
Data$Ph_hat <- as.data.frame(sapply(tmp, function(x){
  x$fit$y - x$fit$residuals
}))
names(Data$Ph_hat) <- pst("k",kseq)

## Compare the k = 1 and k = 2 forecasts
plot(Data$t[itest], Data[[pst("Ph",ihouse)]][itest], type="l")
lines(Data$t[itest], Data$Ph_hat$k1[itest], col=2)
lines(Data$t[itest], Data$Ph_hat$k2[itest], col=3)

## The forecast available for a particular time
dim(Data$Ph_hat)
i <- 5000
plot(Data$t[i+kseq], Data[[pst("Ph",ihouse)]][i+kseq], type="l")
lines(Data$t[i+kseq], Data$Ph_hat[i, ], col=2)
##----------------------------------------------------------------
