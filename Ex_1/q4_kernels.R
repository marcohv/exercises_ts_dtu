## Remove all in memory
rm(ls = list())

## ----Plot a kernel-----------------------------------------------
## Make a kernel function, plot it and play a little with it
## 
## Define the triangular kernel function
##   x_i: the point for which to center
##   x: the data points
##   h: the bandwidth
tri <- function(x_i, x, h){ 
    val <- 1 - abs((x - x_i) / h)
    val[val < 0] <- 0
    return(val)
}

## Make a sequence
x = seq(-1 , 1, len = 100)

## See the shape
plot(x, tri(x_i=0, x=x, h=0.4))
## Try to change the parameters (x_i and h) and add it to the plot
points(x, tri(x_i=0, x=x, h=0.4), col = 2)
## Add some more



## ----Play around------------------------------------------------
## Make a kernel function, plot it and play a little with it
## 
## Define an Epanechnikov kernel function
epanechnikov <- function(x_i, x, h)
{
  ## Epanechnikov kernel
  u <- abs(x- x_i)
  u <- u / h
  val <- 3/4 * (1 - u^2)
  ## Set values with |u|>1 to 0
  val[abs(u)>1] <- 0
  return(val)
}

## See the shape
plot(x, epanechnikov(x_i=0, x=x, h=0.4))
## Try to change the parameters (x_i and h) and add it to the plot
points(x, epanechnikov(x_i=0, x=x, h=0.4), col = 2)
## Add some more