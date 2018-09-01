plotTSStairs <- function(x,y,useLines=FALSE,...)
  {
    ## Helping function for stair plotting
    ## Adds a point in the beginning of the stair plot taking the length...
    #x <- seq(asP("2010-01-01"),asP("2010-01-02"),len=10)
    #y <- 1:10
    x <- rep(x,each=2)
    y <- rep(y,each=2)

    x <- c(x[1]-(x[3]-x[1]), x)
    y <- c(y,y[length(y)])
    if(useLines) lines(x,y,...)
    else plot(x,y,type="l",...)
  }
