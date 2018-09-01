## Setup -------------------------------------------------------------------
rm(list=ls())

## Set working dir.
setwd(".") 

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
X <- prepare_data(readSeries("X5", Ts=10, nlags=NA))

## Plot it
plotmulti(X, c("yTi","Te","Qi","Gv"), xnm="timedate", ylab=c("Ti (c)","Te (C)","Qi (W)", "Gv (W/m2)"))



## Balancing solar gain ----------------------------------------------------

## Two-state model with solar gain assigned to Ti
fitTiTw_X <- sdeTiTw(X)
analyzeFit(fitTiTw_X)

## Two-state model with solar gain balanced between Ti and Tw
fitTiTw_GinTw_X <- sdeTiTw_GinTw(X)
analyzeFit(fitTiTw_GinTw_X)



## Assign Qi to different states  ------------------------------------------

## Have two states in the wall and heating goes to wall
fitTiTw2_QinTw_X <- sdeTiTw2_QinTw(X)
analyzeFit(fitTiTw2_QinTw_X)

## Have two states in the wall and a state for the heating
fitTiThTw2_X <- sdeTiThTw2(X)
analyzeFit(fitTiThTw2_X)



# Create a better model ----------------------------------------------------

############
# Below is the model from the function sdeTiTw_sigmalevels. Use it as a starting
# point, and include two layers in the wall and a balancing parameter, p, to
# balance the solar radiation between Ti and Tw.
############

## Generate a new object of class ctsm
model <- ctsm$new()
## Add system equations and thereby also states
model$addSystem(dTi ~ ( 1/(Ci*Riw)*(Tw-Ti) + Aw/Ci*Gv + 1/Ci*Qi )*dt + (1+(stepQi*sigmalevel))*exp(p11)*dw1)
model$addSystem(dTw ~ ( 1/(Cw*Riw)*(Ti-Tw) + 1/(Cw*Rwe)*(Te-Tw) )*dt + (1+(stepQi*sigmalevel))*exp(p22)*dw2)
## Set the names of the inputs
model$addInput(Te,Gv,Qi,stepQi)

## Set the observation equation: Ti is the state, yTi is the measured output
model$addObs(yTi ~ Ti)
## Set the variance of the measurement error
model$setVariance(yTi ~ exp(e11))

## Set the initial value (for the optimization) of the value of the state at the starting time point
model$setParameter(  Ti = c(init=35  ,lb=10    ,ub=45) )
model$setParameter(  Tw = c(init=34  ,lb=10    ,ub=45) )
## Set the initial value of the parameters for the optimization
model$setParameter(  Ci = c(init=1E5 ,lb=1E4   ,ub=1E7) )
model$setParameter(  Cw = c(init=5E5 ,lb=1E4   ,ub=1E8) )
model$setParameter( Riw = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter( Rwe = c(init=0.1 ,lb=1E-5  ,ub=10) )
model$setParameter(  Aw = c(init=0.2 ,lb=0.001 ,ub=3))
model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=10) )
model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )
model$setParameter( sigmalevel= c(init=1  ,lb=0    ,ub=100) )

## Run the parameter optimization
fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)
fit$data[[1]] <- X
fit$Rnames <- c("Rwe","Riw")