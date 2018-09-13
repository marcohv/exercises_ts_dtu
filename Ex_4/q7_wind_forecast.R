##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Packages used
require(splines)

## Source functions
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Make a data.frame with synced observations and NWPs
k <- 24
X <- data.frame(t=Data$t, Pw=Data$Pw, Ws=lag_vector(Data$Wsnwp[ ,pst("k",k)],k))

## Divide into a training set and a test set
## Just keep the indexes
tstart <- "2010-12-01"
tstart_train <- "2011-03-01"
tend <- "2011-04-01"
X <- X[per(tstart,X$t,tend), ]
itrain <- which(per(tstart,X$t,tstart_train))
itest <- which(per(tstart_train,X$t,tend))

## Plot
plotmulti(X, c("Pw|Ws"))

## See the scatter plot
plot(X$Ws, X$Pw)

## Fit a linear regression model
fit <- lm(Pw ~ Ws, X[itrain, ])
abline(fit)

## RMSE on test set
X$Pw_hat_lm <- predict(fit, X)
## Chop below 0
X$Pw_hat_lm[X$Pw_hat_lm < 0] <- 0
X$residuals_lm <- X$Pw - X$Pw_hat_lm
## The score
rmse(X$residuals_lm[itest])

plotmulti(X[itest, ], c("Pw$|Pw_hat_lm"))
##----------------------------------------------------------------

##----------------------------------------------------------------
## Use a spline model to make it non-linear in the wind speed
fit_bslm <- lm(Pw ~ bs(Ws, df=8), X[itrain, ])
X$Pw_hat_bslm <- predict(fit_bslm, X)
X$residuals_bslm <- X$Pw - X$Pw_hat_bslm

## Improvements?
rmse(X$residuals_bslm[itest])
rmse(X$residuals_lm[itest])

## See the forecasts
plotmulti(X[itest, ], c("Pw$|Pw_hat"))
##----------------------------------------------------------------



##----------------------------------------------------------------
## Base spline model with rls
obj <- function(prm, frml, data, k, ieval = 1:nrow(data)) {
    print(prm)
    ## Apply a low-pass filter on the input
    lambda <- prm[1]
    fit <- rls(as.formula(frml), lambda, data, k)
    ## Evaluate only on the ieval rows
    print(score <- rmse(fit$residuals[ieval]))
    return(score)
}

frml <- "Pw ~ bs(Ws, df=8)"

## To have a "burn-in" period, then set ieval (here remove the first 14 days
ieval <- itrain[-1:-(24*14)]
result <- optimize(obj, lower = 0.95, upper = 1, frml = frml, data = X[itrain, ], k = k, ieval = ieval)
result$minimum

## Calculate the forecasts
fit <- rls(as.formula(frml), lambda = result$minimum, X, k)
X$residuals_bsrls <- fit$residuals

## Plot
tmp <- X[itest, ]
plot(tmp$Pw, type = "l")
lines(tmp$Pw - tmp$residuals_bsrls, col = 2)
lines(tmp$Pw - tmp$residuals_bslm, col = 3)

## Improvements
rmse(X$residuals_bsrls[itest])
rmse(X$residuals_bslm[itest])
##----------------------------------------------------------------


##----------------------------------------------------------------
## What about a kernel model?

## Use this somewhat generalized function (see functions/obj_kernel.R)
obj_kernel

## Define model
frml <- "Pw ~ Ws"
h <- c(Ws=3)
ieval <- itrain[-1:-(24*14)]
obj_kernel(h, frml, X, k, ieval)

## Optimize
result <- optim(h, obj_kernel, lower = 2, upper = 8, frml = frml, data = X[itrain, ], ieval = ieval, k = k)
result

## Calculate forecasts
Pw_hat <- obj_kernel(h=result$par, frml, X, k, return_yhat = TRUE)
X$residuals_kn <- X$Pw - Pw_hat

## Compare
tmp <- X[itest, ]
plot(tmp$Pw, type = "l")
lines(tmp$Pw - tmp$residuals_bsrls, col = 2)
lines(tmp$Pw - tmp$residuals_bslm, col = 3)
lines(tmp$Pw - tmp$residuals_kn, col = 4)

## Improvements?
rmse(X$residuals_kn[itest])
rmse(X$residuals_bslm[itest])
rmse(X$residuals_bsrls[itest])
##----------------------------------------------------------------
