## ----Initialize-------------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the splines package (it is included in base R)
require(splines)

## Load the data
X <- readRDS("soenderborg_2day.RDS")



## ----Base spline model-----------------------------------------
## Build a model using base splines
##
## The bs() function can be directly used in the formula
fit <- lm(P ~ bs(Te, df=3), X)
## We get a coefficient for each base spline
summary(fit)
## Plot to see the estimated function between external temperature and heat load
plot(X$Te, X$P)
## We need to use predict to see the estimated function
xseq <- seq(min(X$Te), max(X$Te), len=100)
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)))



## ----Degrees of freedom (df)-----------------------------------
## Build a model using base splines
##
## What should the df be?
##
fit <- lm(P ~ bs(Te, df=3), X)
## We get a coefficient for each base spline
summary(fit)
## Plot the relation between external temperature and heat load
plot(X$Te, X$P)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)))



## ----Intercept--------------------------------------------------
## Use an intercept in lm() or in bs() gives nearly the same result
##   using same number of parameters in the model
fit <- lm(P ~ bs(Te, df=3), X)
summary(fit)
## Plot the relation between external temperature and heat load
plot(X$Te, X$P)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)))
## 
fit <- lm(P ~ 0 + bs(Te, df=4, intercept=TRUE), X)
summary(fit)
## We need to use predict to see the estimated function
lines(xseq, predict(fit, newdata=data.frame(Te=xseq)), col=2)
