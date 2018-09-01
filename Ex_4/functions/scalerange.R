scalerange <- function(x,by)
  {
    rng <- range(x,na.rm=TRUE)
    c(rng[1]+by*diff(rng), rng[2]-by*diff(rng))
  }
  
