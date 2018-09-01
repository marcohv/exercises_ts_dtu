## Setup -------------------------------------------------------------------
rm(list=ls())

## Set working dir.
setwd(".") 

## Use the lubridate package
## install.packages("lubridate")
library(lubridate)

## Source functions
sapply(dir("functions",full.names=TRUE), source)

## A list of parameters used in many functions
pg <- list()
## Rerun the models or read from cache?
pg$rerun <- FALSE
pg$cache_dir <- "cache_fits"
## Use first order interpolation when estimating?
pg$firstorder <- TRUE
## Number of threads used, keep at 1
pg$threads <- 1
## Latitude and longitude of the position
pg$latitude <- 37.097083
pg$longitude <- -2.364994

## Load the package
library(ctsmr)

## Averaging period Ts is in minutes
X <- prepare_data(readSeries("X5", Ts=10, nlags=NA), samples_after_Qi_step = 5)

## Plot it
plotmulti(X, c("yTi","Te","Qi","Gv","stepQi"), ylab=c("Ti (c)","Te (C)","Qi (W)","Gv (W/m2)","stepQi"))

## Plot the different solar radiation measuresments
plotmulti(X ,c("sunElevation$","Gh.1|Gv"))

## Set solar radiation to 0 when sun is below horizon
X$Gv[ X$sunElevation < 0 ] <- 0
X$Gh.1[ X$sunElevation < 0 ] <- 0



# Check is Aw change ------------------------------------------------------

## Rename "Gh.1" to "Gh"
names(X)[names(X)=="Gh.1"] <- "Gh"

## Generate a new object of class ctsm
model <- ctsm$new()
## Add system equations and thereby also states
model$addSystem(dTi ~ ( 1/(Ci*Riw)*(Tw-Ti) + Aw/Ci*Gh + 1/Ci*Qi )*dt + (1+(stepQi*sigmalevel))*exp(p11)*dw1)
model$addSystem(dTw ~ ( 1/(Cw*Riw)*(Ti-Tw) + 1/(Cw*Rwe)*(Te-Tw) )*dt + (1+(stepQi*sigmalevel))*exp(p22)*dw2)
model$addSystem(Aw ~ exp(p33)*dw3)
## Set the names of the inputs
model$addInput(Te,Gh,Qi,stepQi)

## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)
## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))

## Set the initial value (for the optimization) of the value of the state at the starting time point
model$setParameter(  Ti = c(init=35  ,lb=10    ,ub=45) )
model$setParameter(  Tw = c(init=34  ,lb=10    ,ub=45) )
model$setParameter(  Aw = c(init=0.2 ,lb=0.001 ,ub=3))
## Set the initial value of the parameters for the optimization
model$setParameter(  Ci = c(init=1E5 ,lb=1E4   ,ub=1E7) )
model$setParameter(  Cw = c(init=5E5 ,lb=1E4   ,ub=1E8) )
model$setParameter( Riw = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter( Rwe = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( p33 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )
model$setParameter( sigmalevel= c(init=1  ,lb=0    ,ub=100) )

## Run the parameter optimization
fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)

## Check model
summary(fit, extended = TRUE)

## Plot the Aw state
dev.off()
par(mfrow=c(2,1))
val <- predict(fit)[[1]]
plot(X$timedate, val$state$pred$Aw, ylab = "Aw", xlab = "Time")
plot(X$sunAzimuth, val$state$pred$Aw, ylab = "Aw", xlab = expression("Sun Azimuth ("*degree*")"))


# Splined gA curve --------------------------------------------------------


## Model with base splines
library(splines)

dev.off()
plot(X$sunAzimuth, X$sunElevation)
abline(h = 0)

## Inset the boundary angles (in radians) below.
azumith.bound <- c(... , ...)

## Create base splines
Xbs <- as.data.frame(bs(X$sunAzimuth,
                        df=4,
                        Boundary.knots=c(azumith.bound[1],azumith.bound[2]),
                        intercept=TRUE))

## Name and assigne to data frame
names(Xbs) <- pst("Gbs",names(Xbs))
X <- cbind(X,Xbs)

## Play around with these parameters
Aw <- c(0.7, 0.2, 1, 1.5)
# Plot spline function
with(subset(X, day(X$timedate) == 29),{
    plot((Aw[1] * ifelse(sunElevation > 0, Gbs1, NA) +
              Aw[2] * ifelse(sunElevation > 0, Gbs2, NA) +
              Aw[3] * ifelse(sunElevation > 0, Gbs3, NA) +
              Aw[4] * ifelse(sunElevation > 0, Gbs4, NA)),
         type = "l", ylab = "y")
})


# Fit model ---------------------------------------------------------------

## Generate a new object of class ctsm
model <- ctsm$new()
## Add system equations and thereby also states
model$addSystem(dTi ~ ( 1/(Ci*Riw)*(Tw-Ti) + 1/Ci*Qi + Gv * (Aw1 * Gbs1 + Aw2 * Gbs2 + Aw3 * Gbs3 + Aw4 * Gbs4)/Ci)*dt + (1+(stepQi*sigmalevel))*exp(p11)*dw1)
model$addSystem(dTw ~ ( 1/(Cw*Riw)*(Ti-Tw) + 1/(Cw*Rwe)*(Te-Tw) )*dt + (1+(stepQi*sigmalevel))*exp(p22)*dw2)
## Set the names of the inputs
model$addInput(Te,Gv,Gbs1,Gbs2,Gbs3,Gbs4,Qi,stepQi)
##
## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)
## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))
##
## Set the initial value (for the optimization) of the value of the state at the starting time point
model$setParameter(  Ti = c(init=35  ,lb=10    ,ub=45) )
model$setParameter(  Tw = c(init=34  ,lb=10    ,ub=45) )
## Set the initial value of the parameters for the optimization
model$setParameter(  Ci = c(init=1E5 ,lb=1E4   ,ub=1E7) )
model$setParameter(  Cw = c(init=5E5 ,lb=1E4   ,ub=1E8) )
model$setParameter( Riw = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter( Rwe = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter(  Aw1 = c(init=0.2 ,lb=0.000001 ,ub=3))
model$setParameter(  Aw2 = c(init=0.2 ,lb=0.001 ,ub=3))
model$setParameter(  Aw3 = c(init=0.2 ,lb=0.001 ,ub=3))
model$setParameter(  Aw4 = c(init=0.2 ,lb=0.001 ,ub=3))
model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )
model$setParameter( sigmalevel= c(init=1  ,lb=0    ,ub=100) )
## Run the parameter optimization
fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)

## Check model
summary(fit, extended = TRUE)

fit$data[[1]] <- X
fit$Rnames <- c("Riw","Rwe")
analyzeFit(fit)

# Look at the gA curve ----------------------------------------------------


# Save one-step predictions
val <- predict(fit)[[1]]

## Plot gA curve
dev.off()
xseq <- seq(1, 5, len = 100)
Xbs_seq <- bs(xseq, df=4, Boundary.knots=c(1,5), intercept=TRUE)
plot(xseq * 180 / (pi), Xbs_seq %*% fit$xm[c("Aw1","Aw2","Aw3","Aw4")],
     ylab = "gA splined", xlab = expression("Sun Azimuth ("*degree*")"), xlim = c(0, 360),
     type = "l", ylim = c(0, 0.12))
lines(xseq * 180 / (pi), Xbs_seq %*% (fit$xm[c("Aw1","Aw2","Aw3","Aw4")] +
                                          1.96 * fit$sd[c("Aw1","Aw2","Aw3","Aw4")]),lty="dashed")
lines(xseq * 180 / (pi), Xbs_seq %*% (fit$xm[c("Aw1","Aw2","Aw3","Aw4")] -
                                          1.96 * fit$sd[c("Aw1","Aw2","Aw3","Aw4")]),
      lty="dashed")
