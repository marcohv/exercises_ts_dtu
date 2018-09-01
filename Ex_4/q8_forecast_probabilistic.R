##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Packages used
## install.packages("quantreg")
## install.packages("verification")
require(verification)
require(quantreg)
require(splines)

## Source functions
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Make a data.frame with synced observations and NWPs
k <- 24
X <- data.frame(t=Data$t, Ps=Data$G, G=lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday=Data$tday)
X[ X$tday <= 5 | 18 < X$tday, -1] <- NA

## Divide the period into a training set and a test set
## Just keep the indexes
tstart <- "2011-03-01"
tstart_train <- "2011-04-10"
tend <- "2011-05-01"
X <- X[per(tstart,X$t,tend), ]
itrain <- which(per(tstart,X$t,tstart_train))
itest <- which(per(tstart_train,X$t,tend))

## Plot (see functions/plotmulti.R)
plotmulti(X, c("Ps|G"))

## See the scatter plot
plot(X$G, X$Ps)

## Fit a quantile regression model
tau <- seq(0.05,by=0.15)
fit <- rq(Ps ~ G, tau = tau, data = X[itrain, ])
summary(fit)

## Predict the quantiles
Xrq <- predict(fit, X)

## Plot
colnames(Xrq) <- pst("q",tau)
i <- per("2011-04-11",X$t,"2011-04-14")
tmp <- cbind(X[i, ],Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(X$Ps[itest],Xrq[itest, ]))
crpsDecomposition(tmp[ ,1],  tmp[ ,-1])$CRPS
##----------------------------------------------------------------



##----------------------------------------------------------------
## Use a base spline model with rq
## Use the time of day to calculate base splines and multiply with G
##   in this way the function between Ps and G can change conditional on time of day
## Fit a quantile regression model
fit <- rq(Ps ~ bs(tday,df=5) * G, tau = tau, data = X[itrain, ])
summary(Xrq)

## Predict the quantiles
Xrq <- predict(fit, X)

## Plot
colnames(Xrq) <- pst("q",tau)
i <- per("2011-04-11",X$t,"2011-04-14")
tmp <- cbind(X[i, ],Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(X$Ps[itest],Xrq[itest, ]))
crpsDecomposition(tmp[ ,1],  tmp[ ,-1])$CRPS
##----------------------------------------------------------------



##----------------------------------------------------------------
## (Optional) What about a kernel model?
## Below is pasted from "q6_solar_forecast.R":
## It is left as an optional exercise to "convert" the least squares into a quantile regression
## Need to replace lm() with rq(), and rmse with crpsDecomposition()
## and some other things on the way to make it work...

## Wrap the leave-one-out in a function which returns the score
obj <- function(h, frml, data, k = k, ieval = 1:nrow(data), n_min=10, return_yhat = FALSE){
    ## Keep the output in yhat, only for ieval points
    yhat <- sapply(ieval, function(i){
        ## Check if there is enough points to fit
        if((i-k) < n_min){ return(NA) }
        if(is.na(data$tday[i])){ return(NA) }
        ## Only use values available k steps behind (otherwise future values would be used)      
        ipast <- 1:(i-k)
        ## Only everything before the i'th observation will be included in the fit
        fit <- lm(as.formula(frml), data[ipast, ], weights = epanechnikov(data$tday[i], data$tday[ipast], h=h))
        ## Now predict for the i point (for the k'th horizon)
        predict(fit, newdata = data[i, ])
    })
    ##
    if(return_yhat){
        return(yhat)
    }else{
        ## The score value
        nm <- all.vars(as.formula(frml))[1]
        val <- rmse(data[ieval,nm] - yhat)
        ##
        print(pst("h = ",h,", val = ",val))
        ##
        return(val)
    }
}

frml <- "Ps ~ G"
h <- 3
ieval <- itrain[-1:-(24*14)]
obj(h, frml, X, k, ieval)

result <- optimize(obj, lower = 0.9, upper = 4, frml = frml, data = X[itrain, ], ieval = ieval, k = k)

result

Ps_hat <- obj(h=result$minimum, frml, X, k, return_yhat = TRUE)
X$residuals_kern <- X$Ps - Ps_hat

## Compare
tmp <- X[itest, ]
plot(tmp$Ps, type = "l")
lines(tmp$Ps - tmp$residuals_bs_rls, col = 2)
lines(tmp$Ps - tmp$residuals_bs_lm, col = 3)
lines(tmp$Ps - tmp$residuals_kern, col = 4)

boxplot(X$residuals_kern ~ X$tday, ylim=c(-400,400))

rmse(X$residuals_kern[itest])
rmse(X$residuals_bs_lm[itest])
rmse(X$residuals_bs_rls[itest])
rmse(X$residuals_lm[itest])
##----------------------------------------------------------------
