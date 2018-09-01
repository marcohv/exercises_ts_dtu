lagVec <- function(x,lag)
{
  if(lag>0)
    {
      ## Lag x, i.e. delay x lag steps
      return(c(rep(NA,lag),x[1:(length(x)-lag)]))
    }
  if(lag<0)
    {
      ## Lag x, i.e. delay x lag steps
      return(c(x[(abs(lag)+1):length(x)],rep(NA,abs(lag))))
    }
  if(lag==0)
    {
      ## return x
      return(x)
    }  
}
