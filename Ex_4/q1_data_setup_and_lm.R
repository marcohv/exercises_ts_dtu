##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Source scripts in the functions folder
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Learn how the data is setup

## It is kept in a list
class(Data)

## Which variables
names(Data)

## NWPs are data.frames (i.e. matrices) and observations are vectors
str(Data)

## The observed ambient temperature
Data$Ta
## "t" is time
plot(Data$t, Data$Ta, type = "l")

## Its a vector
class(Data$Ta)

## We also have Numerical Weather Forecasts of ambient temperature

## Arranged in a matrix
class(Data$Tanwp)
dim(Data$Tanwp)
names(Data$Tanwp)

## See the forecast available at i
i <- 100
## The time
Data$t[i]
## The forecast
Data$Tanwp[100, ]

## Plot the observations and the k = 1 step ahead forecast
plot(Data$t, Data$Ta, type = "l")
lines(Data$t, Data$Tanwp$k1, col = 2)

## Make a scatter plot
plot(Data$Ta, Data$Tanwp$k1)

## Also of the k = 24 hour forecast
plot(Data$Ta, Data$Tanwp$k24)

## Wuups, we need to lag the NWPs to match in time
## See how lag_vector() works
x <- c(0,0,1,2,3,0,0)
lag_vector(x, 1)
lag_vector(x, 2)

## Lag the NWPs to match in time
plot(Data$Ta, lag_vector(Data$Tanwp$k1, 1))
plot(Data$Ta, lag_vector(Data$Tanwp$k24, 24))

## Which seems to be most accurate k = 1 or 24 steps ahead?
##----------------------------------------------------------------



##----------------------------------------------------------------
## Now lets make a model and calculate forecasts

## Make a data.frame with synced observations and NWPs
## Take Ph4, which is the load from House 4

## Make the k = 1 steps ahead "design matrix"
X <- data.frame(t = Data$t, Ph = Data$Ph4, Ta = lag_vector(Data$Tanwp$k1,1), G = lag_vector(Data$Gnwp$k1,1))

## Make a training set, first 3 month, and a test set
## Just keep the indexes
itrain <- which(per("2010-09-01",X$t,"2010-12-01"))
itest <- which(per("2010-12-01",X$t,"2011-01-01"))

## Fit a linear model on the training set
fit <- lm(Ph ~ Ta + G, X[itrain, ])

## Are the coefficients significant?
summary(fit)

## Predict 
X$Ph_hat_lm <- predict(fit, X)

## Plot the test set
plot(X$t[itest], X$Ph[itest], type = "l")
lines(X$t[itest], X$Ph_hat_lm[itest], col = 2)

## The score
rmse(X$Ph[itest] - X$Ph_hat_lm[itest])
##----------------------------------------------------------------


