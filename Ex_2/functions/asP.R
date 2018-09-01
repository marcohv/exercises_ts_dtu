asP <- function(timeVal, tz="GMT", ...)
  {
    ## For easy conversion from string or seconds since 1970-01-01 to POSIXct
    switch(class(timeVal[1])[1],
           character=as.POSIXct(timeVal,tz=tz, ...),
           POSIXct=timeVal,
           POSIXlt=timeVal,
           ISOdate(1970,1,1,0)+timeVal
           )
  }

asPlt <- function(timeVal, tz="GMT", ...)
  {
    ## For easy conversion from string or seconds since 1970-01-01 to POSIXlt
    switch(class(timeVal[1])[1],
           character=as.POSIXlt(timeVal,tz=tz, ...),
           POSIXct=as.POSIXlt(timeVal),
           POSIXlt=timeVal,
           as.POSIXlt(ISOdate(1970,1,1,0)+timeVal)
           )
  }
