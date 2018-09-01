## Shift the values in vector x, lag steps to the right
lagWithCycling <- function(x, lag)
  {
    n <- length(x)
    temp <- x[(lag+1):n]
    x[(n-(lag-1)):n] <- x[1:lag]
    x[1:(n-lag)] <- temp
    x
  }

