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

## Define the RMSE score function
rmse <- function(x){
    sqrt(mean(x^2,na.rm=TRUE))
}



## ----Conditional on time t--------------------------------------
## Use t to calculate the weights used in the local fit

## Wrap the leave-one-out cross-validation in a function for an optimizer
obj <- function(h, X){
    P_hat <- rep(NA, nrow(X))
    for(i in 1:nrow(X)){
        i_subset <- (1:nrow(X))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(P ~ Te, data = X, subset = i_subset, weight = tri(X$t[i], X$t, h=h*24*3600))
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = X[i, ])
    }
    ## The score value
    val <- rmse(X$P - P_hat)
    ## Print it
    print(paste0("h = c(",paste(h,collapse=", "),"), val = ",val))
    ## Return the score
    return(val)
}

## Tune the bandwidth
result <- optimize(obj, lower = 1, upper = 100, X = X)
result

## Use the bandwidth
h <- result$minimum

## Fit the model for each time point
coef_intercept <- rep(NA, nrow(X))
coef_Te <- rep(NA, nrow(X))
##
for(i in 1:nrow(X)){
    fit <- lm(P ~ Te, data = X, weight = tri(X$t[i], X$t, h=h*24*3600))
    coef_intercept[i] <- fit$coefficients[1]
    coef_Te[i] <- fit$coefficients[2]
}

## Plot the coefficients as a function of time
par(mfrow = c(2,1))
plot(X$t, coef_intercept)
plot(X$t, coef_Te)



## ----BS conditional---------------------------------------------
## Do the same with base splines
library(splines)

## Wrap it in a function for an optimizer
obj <- function(df, X){
    P_hat <- rep(NA, nrow(X))
    for(i in 1:nrow(X)){
        i_subset <- (1:nrow(X))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(P ~ 0 + bs(t, df=df, intercept=TRUE) + bs(t, df=df, intercept=TRUE):Te, data = X, subset = i_subset)
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = X[i, ])
    }
    ## The score value
    val <- rmse(X$P - P_hat)
    ##
    print(paste("df =",df,", val =",val))
    ##
    return(val)
}

for(df in 4:10){
    obj(df, X)
}

## Or use AIC
for(df in 4:10){
    ## Just fit it
    fit <- lm(P ~ 0 + bs(t, df=df, intercept=TRUE) + bs(t, df=df, intercept=TRUE):Te, data = X)
    ## Calculate the AIC
    print(paste("df =",df,", AIC =",AIC(fit)))
}

## See the estimated function of time
fit <- lm(P ~ 0 + bs(t, df=7, intercept=TRUE) + bs(t, df=7, intercept=TRUE):Te, data = X)
summary(fit)

## Plot them
par(mfrow = c(2,1))
plot(bs(X$t, df=7, intercept=TRUE) %*% fit$coef[1:7], xlab = "Time", ylab = "Intercept")
plot(bs(X$t, df=7, intercept=TRUE) %*% fit$coef[8:14], xlab = "Time", ylab = "Te coefficient")
