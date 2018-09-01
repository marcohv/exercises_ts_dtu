## ----Initialize-------------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the data
X <- readRDS("soenderborg_2day.RDS")

## Load the splines package
library(splines)

## Define the triangular kernel function
tri <- function(x_i, x, h){ 
    val <- 1 - abs((x - x_i) / h)
    val[val < 0] <- 0
    return(as.numeric(val))
}

## The Epanechnikov kernel
epanechnikov <- function(x_i, x, h)
{
    x_i <- as.numeric(x_i)
    x <- as.numeric(x)
    ## Epanechnikov kernel
    u <- abs(x - x_i)
    u <- u / h
    val <- 3/4 * (1 - u^2)
    ## Set values with |u|>1 to 0
    val[abs(u)>1] <- 0
    return(val)
}

## Define the RMSE score function
rmse <- function(x){
    sqrt(mean(x^2,na.rm=TRUE))
}



## ----Second order local fit-------------------------------------
## Extend the model using a second order input (simply square the input)

## Calculate squared terms
X$Te2 <- X$Te^2
X$G2 <- X$G^2
X$Ws2 <- X$Ws^2

## Wrap the leave-one-out cross-validation in a function for an optimizer
obj <- function(h, frml, data, kern){
    P_hat <- rep(NA, nrow(data))
    for(i in 1:nrow(data)){
        i_subset <- (1:nrow(data))[-i]
        ## The i'th observation will not be included in the fit
        fit <- lm(as.formula(frml), data, subset = i_subset, weights = kern(data$Te[i], data$Te, h))
        ## Now predict for the i'th observation
        P_hat[i] <- predict(fit, newdata = data[i, ])
    }
    ## The score value
    val <- rmse(data$P - P_hat)
    ## Print it
    print(paste0("h = c(",paste(h,collapse=", "),"), val = ",val))
    ## Return the score
    return(val)
}

## Tune the bandwidth and keep the results in a list
frml <- "P ~ Te"
optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = tri)
optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = epanechnikov)

frml <- "P ~ Te + Te2 + G + G2 + Ws + Ws2 + Te:Ws"
result <- optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = tri)
result <- optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = epanechnikov)

## Which is the best model?
## Does is seem like one of the kernel function better?



## ----Te conditional on Ws---------------------------------------
## Investigate the effect of Te conditional on Ws
## so how does the coefficient for Te change as a function of Ws?

## First see the estimated mean conditioned on Te and Ws

## Find index of summer and set measurements to NA
isummer <- 151 < as.POSIXlt(X$t)$yday & as.POSIXlt(X$t)$yday <= 273
X[isummer, -1] <- NA

## Plot the relation between external temperature and heat load
plot(X$Te, X$P)

## Use the rgl package for 3d plotting
## install.packages("rgl")
library(rgl)

## Plot the points
open3d() ## Note: Do not use rgl.open()
points3d(X$Te, X$Ws, X$P, size=3, col="red")
aspect3d(c(1,1,1))
axes3d()
title3d(xlab="Te",ylab="Ws",zlab="P")

## Wrap the leave-one-out cross-validation in a function for an optimizer
obj <- function(h, frml, data, kern){
    P_hat <- rep(NA, nrow(data))
    for(i in 1:nrow(data)){
        if(!is.na(data$Ws[i])){
            i_subset <- (1:nrow(data))[-i]
            ## The i'th observation will not be included in the fit
            fit <- lm(as.formula(frml), data, subset = i_subset, weights = kern(data$Ws[i], data$Ws, h))
            ## Now predict for the i'th observation
            P_hat[i] <- predict(fit, newdata = data[i, ])
        }
    }
    ## The score value
    val <- rmse(data$P - P_hat)
    ## Print it
    print(paste0("h = c(",paste(h,collapse=", "),"), val = ",val))
    ## Return the score
    return(val)
}


## Fit a model with Te as input and Ws for local weighting
frml <- "P ~ Te"
result <- optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = epanechnikov)

## Plot and see the function fitted with the optimal bandwidth
h <- result$minimum
## Fit a locally weighted model for a sequence
x = seq(min(X$Te, na.rm=TRUE), max(X$Te, na.rm=TRUE), len=10)
y = seq(min(X$Ws, na.rm=TRUE), max(X$Ws, na.rm=TRUE), len=10)
##
yprd <- outer(x, y, function(x,y){
    sapply(1:length(x), function(i){
        fit <- lm(as.formula(frml), data = X, weight = epanechnikov(y[i], X$Ws, h=h))
        predict(fit, data.frame(Te=x[i], Ws=y[i]))
    })
})    
## 'jet.colors', alternatives see ?rainbow
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
## Use 100 different colors
colors <- jet.colors(100)
## Set the colors for z values
color <- colors[(yprd-min(yprd))/max(yprd)*99.5+1]
## Make a surface with jet colors
surface3d(x, y, yprd, color=color, alpha=1)
## Make a grid surface
surface3d(x, y, yprd, front="lines", back="lines")

## Can you explain the estimated surface?



## ----More-------------------------------------------------------
## Investigate the effect of Te conditional on Ws
## so how does the coefficient for Te change as a function of Ws?

## Use a local weighted model

## Fit a model with Te as input and Ws for local weighting
frml <- "P ~ Te + Ws + G"
result <- optimize(obj, lower = 0.9, upper = 100, frml = frml, data = X, kern = epanechnikov)

## Plot and see the function fitted with the optimal bandwidth
h <- result$minimum

## Fit a locally weighted model for a sequence
Ws_seq = seq(min(X$Ws, na.rm=TRUE), max(X$Ws, na.rm=TRUE), len=50)
##
Te_KI <- sapply(Ws_seq, function(ws){
    fit <- lm(as.formula(frml), data = X, weight = epanechnikov(ws, X$Ws, h=h))
    c(confint(fit)["Te",1], fit$coefficients["Te"], confint(fit)["Te",2])
})

## Plot the coefficient as a function of the Ws
plot(Ws_seq, Te_KI[2, ], type = "l", ylim = range(Te_KI))
lines(Ws_seq, Te_KI[1, ], lty = 2)
lines(Ws_seq, Te_KI[3, ], lty = 2)



## ----Use base splines-------------------------------------------
## Easily!
frml <- "P ~ 0 + bs(Ws,df=4,intercept=TRUE) + bs(Ws,df=4,intercept=TRUE):Te + G"

## Fit the model
Ws_seq = seq(min(X$Ws, na.rm=TRUE), max(X$Ws, na.rm=TRUE), len=50)
##
fit <- lm(as.formula(frml), data = X)

summary(fit)

## Plot the coefficient as a function of the Ws
est <- bs(Ws_seq, df=4, intercept=TRUE) %*% fit$coefficients[6:9]
plot(Ws_seq, est, type = "l")
