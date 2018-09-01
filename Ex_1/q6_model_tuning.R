## ----Initialize-------------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the data
X <- readRDS("soenderborg_2day.RDS")

## Load the splines package
library(splines)

## Define the triangular kernel function
tri <- function(x_i, x, h){ 
    val <- 1 - abs((x - x_i) / h)
    val[val < 0] <- 0
    return(val)
}



## ----Cross-validation-------------------------------------------
## What should the bandwidth h be set to?

## Optimize using cross-validation (leave-one-out):
##   Predict for each observation, WITHOUT using that observation when fitting

## So for observation i, use "subset" argument in lm to include all other observations than i
i <- 10
i_subset <- (1:nrow(X))[-i]
## So i is not in the sequence
i_subset
## The i'th observation will not be included in the fit
fit <- lm(P ~ Te, data = X, subset = i_subset, weight = tri(X$Te[i], X$Te, h=10))
## Now predict for the i'th observation
predict(fit, newdata = data.frame(Te=X$Te[i]))


## Put that in a loop and do it for all the points
P_hat <- rep(NA, nrow(X))
## Use bandwidth
h <- 10
##
for(i in 1:nrow(X)){
    i_subset <- (1:nrow(X))[-i]
    ## The i'th observation will not be included in the fit
    fit <- lm(P ~ Te, data = X, subset = i_subset, weight = tri(X$Te[i], X$Te, h=h))
    ## Now predict for the i'th observation
    P_hat[i] <- predict(fit, newdata = data.frame(Te=X$Te[i]))
}
##
plot(X$Te, X$P)
points(X$Te, P_hat, col = 2)

## Calculate the RMSE score
rmse <- function(x){
    sqrt(mean(x^2,na.rm=TRUE))
}
rmse(X$P - P_hat)



## ----Optimize---------------------------------------------------
## Use and optimizer to find the optimal value of the bandwidth h

## Wrap the leave-one-out in a function which returns the score
obj <- function(h, X){
    P_hat <- rep(NA, nrow(X))
    for(i in 1:nrow(X)){
        i_subset <- (1:nrow(X))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(P ~ Te, data = X, subset = i_subset, weight = tri(X$Te[i], X$Te, h=h))
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = data.frame(Te=X$Te[i]))
    }
    ## The score value
    val <- rmse(X$P - P_hat)
    ##
    print(paste("h =",h,", val =",val))
    ##
    return(val)
}

## Try it
obj(h = 1, X)
obj(h = 10, X)
obj(h = 100, X)

## The function can now be optimized automatically
result <- optimize(obj, lower = 1, upper = 100, X = X)
result



## ----The estimated function-------------------------------------
## Plot and see the function fitted with the optimal bandwidth
h <- result$minimum

## Fit a locally weighted model for a sequence
x = seq(min(X$Te), max(X$Te), len=100)

## A vector for the predictions
P_hat <- rep(NA, length(x))
## Fit and predict for each point
for(i in 1:length(x)){
    fit <- lm(P ~ Te, data = X, weight = tri(x[i], X$Te, h=h))
    P_hat[i] <- predict(fit, newdata = data.frame(Te=x[i]))
}
## Plot it
plot(X$Te, X$P)
lines(x, P_hat)



## ----BS cross-validation----------------------------------------
## For the optimal df for the spline model using cross-validation
##
## Wrap it in a function for an optimizer
obj <- function(df, X){
    P_hat <- rep(NA, nrow(X))
    for(i in 1:nrow(X)){
        i_subset <- (1:nrow(X))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(P ~ bs(Te, df=df), data = X, subset = i_subset)
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = data.frame(Te=X$Te[i]))
    }
    ## The score value
    val <- rmse(X$P - P_hat)
    ##
    print(paste("df =",df,", val =",val))
    ##
    return(val)
}

for(df in 3:10){
    obj(df, X)
}



## ----BS selection score-----------------------------------------
## Or use AIC
for(df in 3:10){
    ## Just fit it
    fit <- lm(P ~ bs(Te, df=df), data = X)
    ## Calculate the AIC
    print(paste("df =",df,", AIC =",AIC(fit)))
}


## Or use BIC (punish larger models more than AIC)
for(df in 3:10){
    ## Just fit it
    fit <- lm(P ~ bs(Te, df=df), data = X)
    ## Calculate the AIC
    print(paste("df =",df,", BIC =",BIC(fit)))
}
