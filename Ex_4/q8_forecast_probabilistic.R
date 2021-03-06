##----------------------------------------------------------------
## Init by deleting all variables and functions
rm(list=ls())
## Set the working directory
setwd(".")

## Packages used
## install.packages("quantreg")
## install.packages("verification")
require(verification)
require(quantreg)
require(splines)

## Source functions
sapply(dir("functions",full.names=TRUE), source)

## Load the data
Data <- readRDS("data_soenderborg.RDS")
##----------------------------------------------------------------


##----------------------------------------------------------------
## Make a data.frame with synced observations and NWPs
k <- 24
X <- data.frame(t=Data$t, Ps=Data$G, G=lag_vector(Data$Gnwp[ ,pst("k",k)],k), tday=Data$tday)
X[ X$tday <= 5 | 18 < X$tday, -1] <- NA

## Divide the period into a training set and a test set
## Just keep the indexes
tstart <- "2011-03-01"
tstart_train <- "2011-04-10"
tend <- "2011-05-01"
X <- X[per(tstart,X$t,tend), ]
itrain <- which(per(tstart,X$t,tstart_train))
itest <- which(per(tstart_train,X$t,tend))

## Plot (see functions/plotmulti.R)
plotmulti(X, c("Ps|G"))

## See the scatter plot
plot(X$G, X$Ps)

## Fit a quantile regression model
tau <- seq(0.05,by=0.15)
fit <- rq(Ps ~ G, tau = tau, data = X[itrain, ])
summary(fit)

## Predict the quantiles
Xrq <- predict(fit, X)

## Plot
colnames(Xrq) <- pst("q",tau)
i <- per("2011-04-11",X$t,"2011-04-14")
tmp <- cbind(X[i, ],Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(X$Ps[itest],Xrq[itest, ]))
crpsDecomposition(tmp[ ,1],  tmp[ ,-1])$CRPS
##----------------------------------------------------------------



##----------------------------------------------------------------
## Use a base spline model with rq
## Use the time of day to calculate base splines and multiply with G
##   in this way the function between Ps and G can change conditional on time of day
## Fit a quantile regression model
fit <- rq(Ps ~ bs(tday,df=5) * G, tau = tau, data = X[itrain, ])
summary(Xrq)

## Predict the quantiles
Xrq <- predict(fit, X)
colnames(Xrq)

## Plot
colnames(Xrq) <- pst("q",tau)
i <- per("2011-04-11",X$t,"2011-04-14")
tmp <- cbind(X[i, ],Xrq[i, ])
plotprob(list(tmp), nm.out = "Ps")

## CRPS score on test set, use the function from the verification package
tmp <- na.omit(cbind(X$Ps[itest],Xrq[itest, ]))
head(tmp)

crpsDecomposition(tmp[ ,1],  tmp[ ,-1])$CRPS
##----------------------------------------------------------------



##----------------------------------------------------------------
## (Optional) What about a kernel model?
## Below is pasted from "q6_solar_forecast.R":
## It is left as an optional exercise to "convert" the least squares into a quantile regression
## Need to replace lm() with rq(), and rmse with crpsDecomposition()
## and some other things on the way to make it work...

## Wrap the leave-one-out in a function which returns the score
obj <- function(h, frml, tau,  data, k = k, ieval = 1:nrow(data), n_min=10, return_yhat = FALSE){
    ## Keep the output in yhat, only for ieval points
    yhat <- sapply(ieval, function(i){
        ## Check if there is enough points to fit
        if((i-k) < n_min){ return(NA) }
        if(is.na(data$tday[i])){ return(NA) }
        ## Only use values available k steps behind (otherwise future values would be used)      
        ipast <- 1:(i-k)
        ## Only everything before the i'th observation will be included in the fit
        fit <- rq(as.formula(frml), 
                  tau = tau, 
                  data = data[ipast, ], 
                  weights = epanechnikov(data$tday[i], data$tday[ipast], h=h))
        ## Now predict for the i point (for the k'th horizon)
        predict(fit, newdata = data[i, ])
    })
    ##
    if(return_yhat){
        return(yhat)
    }else{
        ## The score value
      
            #to select the column of the dataset that 
            #is the response variable "Y" in the formula Y ~ x
        nm <- all.vars(as.formula(frml))[1] 
        
            #data[ieval, nm] subsets the data to the evaluation interval
            # and selects only the column "nm" which is the response variable "Y"
        
            #yhat is a list of matrices that are binded by rows (rbind) into a single matrix
        
            # we cbind the input data column with the forecasted data columns form yhat
            #we omit the NA's
        
        tmp <- na.omit(cbind(data[ieval, 2], do.call(rbind, yhat)))
        
        val <- crpsDecomposition(tmp[ ,1],  tmp[ ,-1])$CRPS
        
        print(pst("h = ",h,", val = ",val))
        ##
        return(val)
    }
}

#define the paramters for the new function
frml <- "Ps ~ G"
h <- 3
tau <- seq(0.05,by=0.15)
ieval <- itrain[-1:-(24*14)]

#apply the function
obj(h, frml, tau, X, k, ieval)

#optimize the base of the kernel "h"
result <- optimize(obj, lower = 0.9, upper = 4,  frml = frml, tau = tau, data = X[itrain, ], ieval = ieval, k = k)

#obtain the optimized h 
result$minimum

#obtian the corresponding optimized CRPS
crps_knqr<- result$objective

#predict for X using the optimized kernel model with rq

Ps_hat <- obj(h = result$minimum, #use the optimized h
              frml, 
              tau = tau,
              X, k, return_yhat = TRUE)

#the result is a list of matrices that need to binded by rows (rbind) into a single matrix
Xrq <- do.call(rbind, Ps_hat ) 


## Plot
colnames(Xrq) <- pst("q",tau) #change the column names
i <- per("2011-04-11",X$t,"2011-04-14") #choose the interval to plot

tmp <- cbind(X[i, ], Xrq[i, ]) # put together the actual values and the predicted values in a matrix
head(tmp)
plotprob(list(tmp), nm.out = "Ps")


