##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Source scripts in the functions folder
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")

## Make the k = 1 "design matrix"
X <- data.frame(t = Data$t, Ph = Data$Ph4, Ta = lag_vector(Data$Tanwp$k1,1), G = lag_vector(Data$Gnwp$k1,1), tday = Data$tday)

## The index of training and test set
itrain <- which(per("2010-09-01",X$t,"2010-12-01"))
itest <- which(per("2010-12-01",X$t,"2011-01-01"))
##----------------------------------------------------------------


##----------------------------------------------------------------
## We know that there are dynamics from Ta to Ph

## The low-pass filter function is defined in "functions/lp_vector.R"

## Apply a low-pass filter on the input
X$Ta_lp <- lp_vector(X$Ta, a1 = 0.99)
## and plot the test set
plot(X$t[itest], X$Ta[itest], type = "l")
lines(X$t[itest], X$Ta_lp[itest], col = 2)

## Use it to make a model
## Fit a linear model on the training set
fit <- lm(Ph ~ Ta_lp + G, X[itrain, ])

## Are the coefficients significant?
summary(fit)

## Predict and plot
X$Ph_hat_lp1 <- predict(fit, X)
##
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_lp1[itest], col = 2)

## The score
rmse(X$Ph[itest] - X$Ph_hat_lp1[itest])

## Are we happy now?

##----------------------------------------------------------------



##----------------------------------------------------------------
## We have to tune the low-pass coefficient, do it 
obj <- function(prm, frml, data) {
    ## Find the inputs to lowpass filter
    ## Just overwrite the column in data
    for(nm in names(prm)){
        data[ ,nm] <- lp_vector(data[ ,nm], a1 = prm[nm])
    }
    ## Fit the model
    fit <- lm(frml, data)
    ## Calculate the objective function
    print(val <- rmse(fit$residuals))
    return(val)
}

frml <- as.formula(Ph ~ Ta + G)

obj(c(Ta=0.98, G=0.98), frml, X[itrain, ])

result <- optim(c(Ta=0.98,G=0.98), obj, lower = c(0.3,0.1), upper = c(0.999,0.999), frml=frml, data=X[itrain, ], method="L-BFGS-B")

result
##----------------------------------------------------------------



##----------------------------------------------------------------
## Now lets analyse the predictions

## Hmm, lets extend our objective function, such that it also can return the predictions
obj <- function(prm, frml, data, itrain, return_fit = FALSE) {
    ## Find the inputs to lowpass filter
    ## Just overwrite the column in data
    for(nm in names(prm)){
        data[ ,nm] <- lp_vector(data[ ,nm], a1 = prm[nm])
    }
    ## Fit the model, ONLY on the training set
    fit <- lm(frml, data[itrain, ])
    ## Calculate the objective function
    print(val <- rmse(fit$residuals))
    ## Either return the fit and more
    if(return_fit){
        return(list(val = val,
                    fit = fit,
                    yhat = predict(fit, data)))
    }else{
        ## Or just return the score
        return(val)
    }
}

## It can be used in optim
result <- optim(c(Ta=0.98,G=0.98), obj, lower = c(0.3,0.1), upper = c(0.999,0.999), frml = frml, data = X[itrain, ], method = "L-BFGS-B", itrain = itrain)

## But now we can also get the fit and the predictions
L <- obj(result$par, frml, X, itrain, return_fit = TRUE)

summary(L$fit)

X$Ph_hat_lpopt <- L$yhat

## Plot the predictions
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_lpopt[itest], col = 2)

## The score
rmse(X$Ph[itest] - X$Ph_hat_lpopt[itest])

## Did we improve?
##----------------------------------------------------------------



##----------------------------------------------------------------
## We miss something: a diurnal curve

## Use base splines
library(splines)

## We have the hour of the day
X$tday

## Hey thats simple now! Add base splines to the formula
frml <- as.formula(Ph ~ bs(tday,df=4) + Ta + G)

## Tune the low-pass coefficients
result <- optim(c(Ta=0.98,G=0.98), obj, lower = c(0.3,0.1), upper = c(0.999,0.999), frml=frml, data=X[itrain, ], method="L-BFGS-B")

result

## Get the fit and the predictions
L <- obj(result$par, frml, X, itrain, return_fit = TRUE)

summary(L$fit)

X$Ph_hat_diur <- L$yhat

##
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_diur[itest], col = 2)

## The score, compare it to the "simpler" models
rmse(X$Ph[itest] - X$Ph_hat_diur[itest])
rmse(X$Ph[itest] - X$Ph_hat_lpopt[itest])
rmse(X$Ph[itest] - X$Ph_hat_lp1[itest])
##----------------------------------------------------------------



##----------------------------------------------------------------
## (Out of scope) We could use Fourier series as base functions

## Fourier series for one day of hourly values
x <- (0:23 + 0.5) / 24
n_harmonics <- 4
L <- lapply(1:n_harmonics, function(i) {
    val <- data.frame(sin(i * x * 2 * pi), cos(i * x * 2 * pi))
    names(val) <- paste0(c("sin_", "cos_"), i)
    return(val)
})
Xtmp <- do.call("cbind", L)
##
par(mfrow = c(2,1))
plot(Xtmp$sin_1, type = "b")
for(i in 2:ncol(Xtmp)) {
    lines(Xtmp[ ,i], col = i, type = "b")
}
##
plot(Xtmp[ ,ncol(Xtmp)], type = "b")
## A linear combination can form any harmonic function
plot(apply(runif(n_harmonics * 2) * t(Xtmp), 2, sum), type = "b")

## How many harmonics make sense to maximum include when the period is in 24 steps?
##----------------------------------------------------------------


##----------------------------------------------------------------
## Fit a diurnal curve with Fourier series as base functions
tmp <- fs(X$tday/24, n_harmonics=3)
tmp <- do.call("cbind", tmp)

Xfit <- cbind(X,tmp)

frml <- as.formula(Ph ~ sin_1 + cos_1 + sin_2 + cos_2 + sin_3 + cos_3 + Ta + G)

## Tune the low-pass coefficients
result <- optim(c(Ta=0.98,G=0.98), obj, lower = c(0.3,0.1), upper = c(0.999,0.999), frml=frml, data=Xfit[itrain, ], method="L-BFGS-B")

result

## But now we can also get the fit and the predictions
L <- obj(result$par, frml, Xfit, itrain, return_fit = TRUE)

summary(L$fit)

X$Ph_hat_fs_diur<- L$yhat

##
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_fs_diur[itest], col = 2)

## The score, compare it to the "simpler" models
rmse(X$Ph[itest] - X$Ph_hat_fs_diur[itest])
rmse(X$Ph[itest] - X$Ph_hat_diur[itest])
rmse(X$Ph[itest] - X$Ph_hat_lpopt[itest])
rmse(X$Ph[itest] - X$Ph_hat_lp1[itest])
##----------------------------------------------------------------
