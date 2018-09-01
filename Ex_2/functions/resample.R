resampleDF <- function(X,Ts,startTime,endTime=NA,timeName="t",includeNA=TRUE,quantizeTime=TRUE,meanNaRm=FALSE)
  {
    ###########################################################################
    ## Resample a dataframe.
    ## It is assumed that the time points represent the past period.
    ## The start time is the left limit of the first sample period.
    
    ###########################################################################
    ## The start time has to be given.
    ## Set the end time if not specified
    if(is.na(endTime)){ endTime <- X[nrow(X),timeName]}
    
    ###########################################################################
    ## Cut out the time period
    X <- X[startTime<X[,timeName] & X[,timeName]<=endTime,]
    ## Remove values with a NA value in time
    X <- X[!is.na(X[,timeName]), ]
    
    ###########################################################################
    ## Split into periods of length Ts, and take the mean of each period
    X[,timeName] <- (as.numeric(X[,timeName], units="secs")-as.numeric(startTime, units="secs"))
    iSplit <- -(X[,timeName] %/% -Ts)
    ## Do the resampling
    Xres <- aggregate(X, list(iSplit), mean, na.rm=meanNaRm)
    ## Remove the "Group" column
    Xres <- Xres[,-1]
    ## Convert time to POSIXct
    Xres[,timeName] <- startTime + Xres[,timeName]

    if(includeNA)
      {
        ## Include intervals with NA in the result
        Xres <- cbind(Xres,iSplit=unique(iSplit))
        iSplit <- 1:-((as.numeric(endTime, units="secs")-as.numeric(startTime, units="secs")) %/% -Ts)
        withNA <- data.frame(iSplit=iSplit)
        Xres <- merge(Xres,withNA,all=TRUE)
        ## Remove the iSplit column
        Xres <- Xres[,-match("iSplit",names(Xres))]
        if(quantizeTime)
          {
            ## Set the time points to the end of each interval
            time <- seq(startTime,by=Ts,length.out=nrow(Xres)) + Ts
            Xres[,timeName] <- time
          }
      }
    
    return(Xres)
  }
