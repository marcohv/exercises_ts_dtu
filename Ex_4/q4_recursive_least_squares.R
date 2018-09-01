##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Exercise on RLS

## Init
rm(list = ls())
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")

## Make a data.frame with synced observations and NWPs
k <- 24
X <- data.frame(t = Data$t, Ph = Data$Ph4, Ta = lag_vector(Data$Tanwp[ ,pst("k",k)],k), G = lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday = Data$tday)

## A function for fitting a recursive least squares estimation
rls <- function(formula, lambda, data, k) {
    ## R build-in function for setting up linear regression model
    mf <- model.frame(formula, data, na.action = na.pass)
    ## The model output
    y <- mf[ ,1]
    ## The design matrix
    X <- model.matrix(formula, mf)

    ## The number of observations
    n <- nrow(X)
    ## The number of parameters
    p <- ncol(X)
    ## Parameter matrix
    Theta <- matrix(as.numeric(NA), nrow = n, ncol = p)
    ## The predictions
    yhat <- as.numeric(rep(NA,n))  
    ## The parameter vector
    theta <- matrix(rep(0,p), ncol = 1)
    
    ## Start value for the parameter covariance P
    P <- 10000 * diag(1, p)
    ## Use the inverse in the RLS
    R <- solve(P)
    
    ## Iterate through and estimate the parameters
    for (i in 1:(n-k)) {
        x <- matrix(X[i, ])
        ## Check for NAs in inputs and output
        if(all(!is.na(x)) & !is.na(y[i])){
            ## Update
            R <- lambda * R + x %*% t(x)
            theta <- theta + solve(R, x) %*% (y[i] - t(x) %*% theta)
            Theta[i, ] <- t(theta)
        }
        ## Predict
        x <- matrix(X[i+k, ])
        if(all(!is.na(x))){
            yhat[i+k] <- t(x) %*% theta
        }
    }
    
    ## Return a list
    L <- list()
    L$residuals <- y - yhat
    L$X <- X
    L$y <- y
    L$Theta <- Theta
    return(L)
}

## Generate some data from a linear model
n <- 200
x <- runif(n)
beta0 <- 2
beta1 <- -3
y <- beta0 + beta1 * x + rnorm(n)

## Try to estimate the parameters
lm(y ~ x)
plot(x, y)

## Change the coefficients and generate more data
x1 <- runif(n)
beta0 <- -2
beta1 <- 3
y1 <- beta0 + beta1 * x1 + rnorm(n)

## Put together in one "design matrix"
X <- data.frame(y=c(y,y1), x=c(x,x1))
plot(X$x, X$y)

## Fit a linear regression on the binded data
lm(y ~ x, X)

## Fit a recursive linear regression on the binded data
val <- rls(y ~ x, lambda = 0.99, data = X, k = 1)

## Plot the tracked parameters
plot.ts(val$Theta)
##----------------------------------------------------------------
