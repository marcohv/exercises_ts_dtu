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
X <- prepare_data(readSeries("X5", Ts=10, nlags=NA), samples_after_Qi_step = 5)

## Plot it
plotmulti(X, c("yTi","Te","Qi","G","stepQi"), xnm="timedate", ylab=c("Ti (c)","Te (C)","Qi (W)","Gv (W/m2)","stepQi"))


## Compare models with and without changing system noise level ------------

## Keep the fits in a list
Fits <- list()

## TiTw with constant system noise
Fits[["TiTw"]] <- sdeTiTw(X)
## Remember to step back and see the acf and cpgram
analyzeFit(Fits[["TiTw"]], type="s")

## TiTw with two leves of system noise
Fits[["TiTw_sigmalevels"]] <- sdeTiTw_sigmalevels(X)
analyzeFit(Fits[["TiTw_sigmalevels"]], type="s")


## Nice features of R and Rstudio  ----------------------------------------

## About keeping the results in a list:
## Easy to extract the same value for all the fits
## E.g. compare the loglikelihoods
sapply(Fits, function(fit){ fit$loglik })

## Shortcuts for running the code in different ways in RStudio:
## See the menu around "Code->Run Region"

## Setting a break point in a function:
## Open "functions/sdeTiTw.R" and click to the left of the "3" number indicating line 3
## A red circle will appear indicating a breakpoint
## Above the script, click on "Source on Save" to check it
## Press the disc sign to save
## Now go back and run the line with sdeTiTw(X)
## The execution stops and you can use the bottoms above the console to step through the code

## Caching results:
## Look into the "functions/TiTw.R" in the first lines the cache_load() is called
## Go into "functions/cache_load.R", can you figure out what it does?
## Now run
sdeTiTw(X, rerun=TRUE)
## and try to change the value of return between TRUE and FALSE
## The results are cached in the folder "cache_fits", such that if a model was previously
##   run on some data, instead of running again, it just opens the saved result from last time

## Keeping results in a list:
## You kan easily do the same thing to all the fits, e.g. pick a value or do a plot to sum up results
## E.g. compare the loglikelihoods
sapply(Fits, function(fit){ fit$loglik })

