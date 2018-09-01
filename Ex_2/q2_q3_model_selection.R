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

## Read the data into a data.frame
X <- read.csv("inputPRBS1.csv",sep=";",header=TRUE)
X$timedate <- asP("2009-02-05 14:26:00)") + X$t * 3600
##----------------------------------------------------------------


##----------------------------------------------------------------
## The simple model and the one step extended models

## First go and open the file "functions/Ti.R". The model Ti is defined in there.

## Run the parameter optimization for the first model
fitTi <- Ti(X)
## Analyze the results (If you want in RStudio not to have external plot windows, change "newdev", maybe the default value in "functions/analyzeFit.R")
analyzeFit(fitTi, newdev=TRUE)
##----------------------------------------------------------------

##----------------------------------------------------------------
## Run the parameter optimization
fitTiTe <- TiTe(X)
## Analyze the results
analyzeFit(fitTiTe)

## Run the parameter optimization
fitTiTh <- TiTh(X)
## Analyze the results
analyzeFit(fitTiTh)

## Run the parameter optimization
fitTiTm <- TiTm(X)
## Analyze the results
analyzeFit(fitTiTm)

## Run the parameter optimization
fitTiTs <- TiTs(X)
## Analyze the results
analyzeFit(fitTiTs)
##----------------------------------------------------------------


##----------------------------------------------------------------
## Question 3:

## Which of the extensions have the highest likelihood?
fitTiTe$loglik
fitTiTh$loglik
fitTiTm$loglik
fitTiTs$loglik

## Perform a likelihood ratio test: lambda = lik(smallerModel)/lik(largerModel) ,
## where the smallerModel is submodel of the largerModel and lambda is chi2(f)
## distributed with f=dim(smallerModel)-dim(largerModel). Page 20 in Madsen2006.

## Take the results of both models
small <- fitTi
large <- fitTiTh
## Calculate the logLikelihood for both models from their fit
logLikSmallModel <- small$loglik
logLikLargeModel <- large$loglik
## Calculate lambda which follows the chisquare distribution
chisqStat <- -2 * (logLikSmallModel - logLikLargeModel)
## It this gives a p-value smaller than confidence limit, i.e. 5\%, then the
## larger model is significant better than the smaller model
prmDiff <- large$model$NPARAM - small$model$NPARAM
## The p-value of the test, if it is <0.05 the larger model should be preferred over the smaller
1 - pchisq(chisqStat, prmDiff)
##----------------------------------------------------------------

##----------------------------------------------------------------
## liklihood ratio test wrapped in a function
likRatioTest(fitTiTh, fitTi)
##----------------------------------------------------------------

##----------------------------------------------------------------
## PROCEED the forward selection
## The models which could be used in the next step are implemented in the following functions
## Fit different models extended from TiTh
fitTiThTe <- TiThTe(X)
## Analyze the results
analyzeFit(fitTiThTe)

## Fit different models extended from TiTh 
fitTiThTs <- TiThTs(X)
## Analyze the results
analyzeFit(fitTiThTs)

## Fit different models extended from TiTh
fitTiThTm <- TiThTm(X)
## Analyze the results
analyzeFit(fitTiThTm)

## Which one of the three extended models should we select?
## Take the one with the highest likelihood, as in previous step.
fitTiThTe$loglik
fitTiThTs$loglik
fitTiThTm$loglik

## Check that the extension has a significant increase in the loglikelihood function
likRatioTest(fitTiThTe, fitTiTh)

## From here we should keep on extending the model, but for now no larger linear models are implemented here.
## From this points it is also likely that extensions to linear models compensate for non-linear or time-dependent effects.
## See the article "Identifying suitable models for heat dynamics", it is included in the .zip file, the performance (i.e. ACF(e_k) etc.) '
## doesn't really change for models larger than TiThTe compared to the larger tested linear model. 
## Hence we should rather look in residuals for non-linear or transformations of the inputs in order to model 
## the effects which are not described well in the current model.
##----------------------------------------------------------------



##----------------------------------------------------------------
## The estimated HLC-value for the TiThTe model
i <- which(names(fitTiThTe$xm) %in% c("Rea","Rie"))
HLC <- 1 / sum(fitTiThTe$xm[i])
HLC * 1000 ## W/C
## The covariance for the two estimated R values
cov <- diag(fitTiThTe$sd[i]) %*% fitTiThTe$corr[i,i] %*% diag(fitTiThTe$sd[i])

## Calculate the uncertainty of the HLC value with a linear approximation to the covariance
## The Jacobian, the derived of the HLC-value with respect to each estimate in fitTiThTe$xm[i]
J <- t( sapply(1:length(i), function(ii,x){ -1/sum(x)^2 }, x = fitTiThTe$xm[i]) )
## The estimated variance of HLC
varHLC <- J %*% cov %*% t(J)    
## and standard deviance
sdHLC <- sqrt(varHLC)
## Return the confidence interval
c(HLC-1.96*sdHLC,HLC+1.96*sdHLC)*1000


## Calculate the uncertainty of the HLC value with a simulation approach
## Needed for multivariate normal distribution simulation
require(MASS)
## Generate multivariate normal random values
Rsim <- mvrnorm(n=1000000,mu=fitTiThTe$xm[i],Sigma=cov)
## For each realization calculate the HLC-value
HLCsim <- 1/apply(Rsim,1,sum)
## Estimate the 2.5% and 97.5% quantiles of the simulated values as a confidence interval
quantile(HLCsim,probs=c(0.025,0.975))*1000
##----------------------------------------------------------------