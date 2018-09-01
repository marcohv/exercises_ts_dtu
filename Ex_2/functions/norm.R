norm <- function(x,rng=NA)
{
  x <- (x - min(x,na.rm=TRUE)) / max(x-min(x,na.rm=TRUE),na.rm=TRUE)
  if(length(rng)>1)
    {
      rng <- range(rng,na.rm=TRUE)
      x <- min(rng) + x*(max(rng)-min(rng))
    }
  return(x)
}

