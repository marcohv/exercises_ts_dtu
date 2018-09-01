##----------------------------------------------------------------
## Set the working directory
setwd(".")
## Source the files in the "functions" folder
files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])
##----------------------------------------------------------------

##----------------------------------------------------------------
## Question 1
## Read the data into a data.frame
tmp <- read.csv("inputPRBS1.csv",sep=";",header=TRUE)
tmp$timedate <- asP("2009-02-05 14:26:00)") + tmp$t * 3600
##
X <- tmp
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
## Generate a PRBS signal
## Use the function defined in the file "functions/prbs.R", which generates a PRBS signal
## - n is the length of the register
## - initReg just needs to be some initial value of 1,2,...
##   it is the initial value of the registers and therefore only
##   determines the start of the cycle
## - lambda is the length of the smallest period in
##   which the signal can change, given in samples
x <- prbs(n=6)
acf(x, lag.max=length(x))
##  prbs
par(mfrow=c(3,1))
plot(x,type="s")
acf(x, lag.max=length(x))
## See  to PRBS
x <- prbs(n=6)
## LagWithCycling to the right
y <- lagWithCycling(x, lag=10)
## cross cor.
ccf(x,y, lag.max=length(x))

##----------------------------------------------------------------
## Generate the PRBS signals for the PRBS1 experiment, where a single signal controls all heaters in the building
## PRBS med n=6, lambda=4
##   - Smallest period in one state for 5 minute sample period is then 4*5min=20min
##   - Settling time of the system (T_s in the Godfrey 1980 paper) below the period (T_0 in the paper) in: lambda * (2^n-1) * 5 / 60 = 21 hours
x <- prbs(n=6, initReg=666, lambda=4)
x <- rep(x,2)
## Concatenate PRBS med n=5, lambda=36:
##   - Smallest period in one state: 36*5min/60 = 3h
##   - Settling time below: 36 * (2^5-1) * 5 / 60 = 93 hours
y <- prbs(n=5, initReg=666, lambda=36)
##  NOTE high ACF due to lambda>1 (Simply due to repeated values)
acf(y, lag.max=length(y))
## Concatenate the two PRBS
x <- c(x,y)
## Plot it
plot(1:length(x),x,type="s")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Just for seeing how to make multiple PRBS signals, which are uncorrelated
## Multiple PRBS signals are simply generated lagging the signals, such that they do not begin at the same time point
x1 <- prbs(n=6, initReg=666, lambda=4)
n <- length(x1)
x2 <- lagWithCycling(x1, lag=round(n/3))
x3 <- lagWithCycling(x1, lag=round(2*n/3))
## Check them
ccf(x1,x2, lag.max=n)
ccf(x1,x3, lag.max=n)
## repeat them
x1 <- rep(x1,2)
x2 <- rep(x2,2)
x3 <- rep(x3,2)
## The long period
x1.long <- prbs(n=5, initReg=667, lambda=36)
n <- length(x1.long)
x2.long <- lagWithCycling(x1.long, lag=round(n/3))
x3.long <- lagWithCycling(x1.long, lag=round(2*n/3))
## Check them
ccf(x1.long,x2.long, lag.max=n)
ccf(x1.long,x3.long, lag.max=n)
## Concatenate them
x1 <- c(x1,x1.long)
x2 <- c(x2,x2.long)
x3 <- c(x3,x3.long)
## Plot them
setpar("ts",mfrow=c(3,1))
plot(x1,type="s")
plot(x2,type="s")
plot(x3,type="s")
axis(side=1,xaxt="s")
##----------------------------------------------------------------

