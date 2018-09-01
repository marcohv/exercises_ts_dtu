##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Source the scripts with functions in the "functions" and "models" folder
files <- dir("functions", full.names=TRUE)
for(i in 1:length(files)) source(files[i])

## Use the ctsmr package
library(ctsmr)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Plot the data
## Read the data into a data.frame
X <- read.csv("inputPRBS1.csv",sep=";",header=TRUE)
X$timedate <- asP("2009-02-05 14:26:00)") + X$t * 3600
## Plot the time series (see "functions/setpar.R" to see how the plot is setup)
## Keep the default plotting parameters for resetting the plotting device below
def.par <- par(no.readonly=TRUE)
##
setpar("ts",mfrow=c(4,1))
gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
## 
plot(X$timedate,X$yTi,type="n",ylab="yTi")
abline(v=gridSeq,h=0,col="grey92")
lines(X$timedate,X$yTi)
## 
plot(X$timedate,X$Ta,type="n",ylab="Ta")
abline(v=gridSeq,h=0,col="grey92")
lines(X$timedate,X$Ta)
## 
plot(X$timedate,X$Ph,type="n",ylab="Ph")
abline(v=gridSeq,h=0,col="grey92")
lines(X$timedate,X$Ph)
## 
plot(X$timedate,X$Ps,type="n",ylab="Ps")
abline(v=gridSeq,h=0,col="grey92")
lines(X$timedate,X$Ps)
##
plotTSXAxis(X$timedate,format="%Y-%m-%d")
## Set the plotting parameters to default again
par(def.par)
layout(1)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Generate a new object of class ctsm.
model <- ctsm$new()
## "ctsm" is a class, which has fields and methods, which are used for carrying out the modelling
class(model)
## See the fields and methods
ctsm
## or
str(ctsm)
str(model)

## We can now access a field, e.g. to see that there are no state variables defined
model$states

## Now we can add a system equation (and thereby also a state variable) with the addSystem() function
model$addSystem(dTi ~ ( 1/(Ci*Ria)*(Ta-Ti) + Aw/Ci*Ps + 1/Ci*Ph )*dt + exp(p11)*dw1)

## Note that the deterministic part of the SDE is multiplied with dt.
## Note that the stochastic part is multiplied with system noise process dw1
## Note that the variance of the system noise is exp(p11), where exp() is the exponential function and
##   p11 is the parameter. Since the variance is strictly positive, but can be very close to zero, it
##   is a good idea to take exp() of the parameter, since then p11 can go from -Inf to Inf but
##   the exponential goes from 0 to Inf, with good resolution towards 0.

## Now we can see that there is one state variable is defined
model$states

## Set the names of the inputs (simply the same as in the data.frame used for estimation below)
model$addInput(Ta,Ps,Ph)

## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)

## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))

## Set the initial value of the value of the state at the start time (values where
## the estimation (i.e. optimization of the likelihood) starts) and also the lower
## and upper bound, which must contain the parameter value
model$setParameter(  Ti = c(init=15  ,lb=0     ,ub=25 ) )

## Set the initial values and bounds for the optimization of the parameters
model$setParameter(  Ci = c(init=1   ,lb=1E-5  ,ub=20 ) )
model$setParameter( Ria = c(init=20  ,lb=10    ,ub=1E4) )
model$setParameter(  Aw = c(init=20  ,lb=1     ,ub=300) )
model$setParameter( p11 = c(init=1   ,lb=-30   ,ub=10 ) )
model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10 ) )
##----------------------------------------------------------------


##----------------------------------------------------------------
## Take a break here start on the points in the exercise document

## Run the parameter estimation
fit <- model$estimate(X)
## Note that for the estimation the following could be set (the default values fits the current case):
##  firstorderinputinterpolation = FALSE, means zero order hold of the inputs between the sample points
##  threads = 1, the optimization can use multiple threads (can create some crash issues)

## See the summary of the estimation
summary(fit)
##----------------------------------------------------------------


##----------------------------------------------------------------
## If any of the parameter estimates are close to the lower or upper bound then "dF/dPar" is
## significant compered to "dPen/dPar"
summary(fit, extended = TRUE)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Calculate the one-step predictions of the state (i.e. the residuals)
tmp <- predict(fit)[[1]]
str(tmp)

## Calculate the residuals and put them with the data in a data.frame X
X$residuals <- X$yTi - tmp$output$pred$yTi

## Plot the auto-correlation function and cumulated periodogram in a new window
par(mfrow=c(1,3))
## The blue lines indicates the 95% confidence interval, meaning that if it is
##  white noise, then approximately 19 out of 20 lag correlations will be inside.
acf(X$residuals, lag.max=8*24)
## The periodogram is the estimated energy spectrum in the signal
spec.pgram(X$residuals)
## The cumulated periodogram 
cpgram(X$residuals)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Time series plots of the inputs and residuals
tmp <- X
## Or take only a period, e.g. the two first days
##tmp <- X[per(tstart,X$timedate,tstart+2*24*3600),]

## Plot the residuals
## Prepare a time series plot (see "functions/setpar.R")
setpar("ts",mfrow=c(5,1))
gridSeq <- seq(asP("2009-01-01"),by="days",len=365)
##
plot(tmp$timedate,tmp$residuals,type="n",ylab="residuals")
abline(v=gridSeq,h=0,col="grey92")
lines(tmp$timedate,tmp$residuals)
title(main=as.character(match.call())[2],line=-2,cex.main=2)
## 
plot(tmp$timedate,tmp$Ph,type="n",ylab="Ph")
abline(v=gridSeq,h=0,col="grey92")
lines(tmp$timedate,tmp$Ph)
## 
plot(tmp$timedate,tmp$yTi,type="n",ylab="yTi")
abline(v=gridSeq,h=0,col="grey92")
lines(tmp$timedate,tmp$yTi)
lines(tmp$timedate,tmp$yTi-tmp$residuals,col=2)
legend("bottomright",c("Measured","Predicted"),lty=1,col=1:2,bg="grey95")
## 
plot(tmp$timedate,tmp$Ta,type="n",ylab="Ta")
abline(v=gridSeq,h=0,col="grey92")
lines(tmp$timedate,tmp$Ta)
##
plot(tmp$timedate,tmp$Ps,type="n",ylab="Ps")
abline(v=gridSeq,h=0,col="grey92")
lines(tmp$timedate,tmp$Ps)
##
plotTSXAxis(tmp$timedate,format="%Y-%m-%d")
## Set the plotting parameters to default again
par(def.par)
layout(1)
##----------------------------------------------------------------


