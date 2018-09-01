## ----Initialize-------------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the data
X <- readRDS("soenderborg_2day.RDS")

## Define the triangular kernel function
tri <- function(x_i, x, h){ 
    val <- 1 - abs((x - x_i) / h)
    val[val < 0] <- 0
    return(val)
}



## ----Locally weighted-------------------------------------------
## Fit a locally weighted model for some point 
x_i <- 5

## Fit the model using the kernel as weights (hence weighted linear regression model)
fit <- lm(P ~ Te, data = X, weight = tri(x_i, X$Te, h=10))
predict(fit, newdata = data.frame(Te=x_i))



## ----Bandwidth--------------------------------------------------
## Fit a locally weighted model for a sequence
x = seq(min(X$Te), max(X$Te), len=100)

## The bandwidth
h <- 10
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

## The question is now: What should the bandwidth h be?
