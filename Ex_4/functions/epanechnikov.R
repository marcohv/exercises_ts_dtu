epanechnikov <- function(x_i, x, h)
{
    if(class(x_i)[1] != "numeric"){
        x_i <- as.numeric(x_i)
        x <- as.numeric(x)
    }
  ## Epanechnikov kernel
  u <- abs(x - x_i)
  u <- u / h
  val <- 3/4 * (1 - u^2)
  ## Set values with |u|>1 to 0
  val[abs(u)>1] <- 0
  return(val)
}
