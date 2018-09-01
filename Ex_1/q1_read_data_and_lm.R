## ----Initialize-----------------------------------------------
## Remember to set the working directory to the folder where you 
## have the data and scripts
##
## Initialize by removing all variables in memory
rm(list=ls())



## ----Read data------------------------------------------------
## Read the data into a data.frame. The data consists of hourly average values
Xorig <- read.table("soenderborg_2day.csv", sep=",", header=TRUE, as.is=TRUE)
## Convert the time from character to POSIXct, which is the class in R for representing time
Xorig$t <- as.POSIXct(Xorig$t, tz="GMT")

## Set which house to use (there are 3 in the data)
ihouse <- 3
Xorig$P <- Xorig[ ,paste0("P",ihouse)]

## Check the content
str(Xorig)
summary(Xorig)

## Save it in R data format for later easy use
saveRDS(Xorig, "soenderborg_2day.RDS")

## Keep it in X
X <- Xorig



## ----Only winter period----------------------------------------
## Set the summer period values to NA

## Find the day of year for starting and ending summer
as.POSIXlt("2011-06-01")$yday
as.POSIXlt("2011-10-01")$yday
## Find index of summer and set measurements to NA
isummer <- 151 < as.POSIXlt(X$t)$yday & as.POSIXlt(X$t)$yday <= 273
X[isummer, -1] <- NA

## Plot the relation between external temperature and heat load
plot(X$Te, X$P)

## Simplest linear model
fit_winter <- lm(P ~ Te, X)
summary(fit_winter)
abline(fit_winter)

## Model validation (check i.i.d. and distribution of residuals)
par(mfrow = c(2,2))
## Plot residuals
plot(fit_winter$residuals)
## Residulas vs. input Te
plot(X$Te[as.integer(names(fit_winter$residuals))], fit_winter$residuals)
hist(fit_winter$residuals)
qqnorm(fit_winter$residuals)
qqline(fit_winter$residuals)

## And of course ACF
par(mfrow = c(1,1))
acf(fit_winter$residuals)



## ----Both summer and winter-------------------------------------
## But what if we don't remove the summer period?
X <- Xorig
plot(X$Te, X$P)

## Simplest linear model
fit <- lm(P ~ Te, X)
summary(fit)
abline(fit)

## Model validation (check i.i.d. and distribution of residuals)
par(mfrow = c(2,2))
## Plot residuals
plot(fit$residuals)
## Residuals vs. input Te
ival <- as.integer(names(fit$residuals))
plot(X$Te[ival], fit$residuals)
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)

## And of course ACF
par(mfrow = c(1,1))
acf(fit_winter$residuals)

## Another great plot to check residuals
X$residuals <- NA
X$residuals[ival] <- fit$residuals
pairs(X[c("residuals","t","Te")], panel = panel.smooth, lower.panel = NULL)
