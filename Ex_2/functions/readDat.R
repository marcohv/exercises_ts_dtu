readDat <- function(tstart,tend,resampleDeltaT)
  {
    ## Read the 2 minute data from June and February
    tmp <- rbind(read.csv("DatJune.csv",stringsAsFactors=FALSE),read.csv("DatFeb.csv",stringsAsFactors=FALSE))
    tmp$t <- asP(tmp$t)
    ## Cut out a period
    tmp <- tmp[per(tstart,tmp$t,tend),]
    ## Resample
    tmp <- resampleDF(tmp, resampleDeltaT, tstart)
    ## Make the time into seconds, keep the POSIXct in another column
    tmp$time <- tmp$t
    tmp$t <- asSeconds(tmp$time - tstart)
    ## Specify the data for the estimation
    Dat1 <- list(data=tmp,sampletime=0,interpolation=0)
  }
