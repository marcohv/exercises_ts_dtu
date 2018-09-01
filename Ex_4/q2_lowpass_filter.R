##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")
##----------------------------------------------------------------


##----------------------------------------------------------------
## In order to take dynamics into account we can filter the inupts
## One can say it is a transformations of the inputs (like with base splines).

## First in order to model a linear dynamical 1st order system (i.e. single RC)
## Apply a low-pass filter

## Make a sequence like an on/off signal
x <- rep(c(rep(0,100),rep(1,100)), 2)
plot(x, type="l")

## Do a first order low-pass filter with coefficient a1
a1 <- 0.99
## Init
y <- x
y[1] <- x[1]
for (i in 2:length(x)) {
    y[i] <- a1 * y[i - 1] + (1 - a1) * x[i]
}

## Plot them
lines(y, col = 2)
##----------------------------------------------------------------

