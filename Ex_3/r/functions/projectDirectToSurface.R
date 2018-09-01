##--------
## Project to an east, south, and west surface
projectDirectToSurface <- function(Ib,slope,pAzimuth,latitude,longitude,maxOut=1E10)
  {
    ## Project to a given surface
    ## All angle are given in degrees.
    ## slope: is the angle between the normal to the ground surface, and the normal of the panel.
    ## pAzimuth: azimuth of the panel where 0 degrees is due south. + is toward west, - is toward east.
##    browser()
    x <- X[,c("t","sunElevation")]
    x$sunZenith <- pi/2-x$sunElevation
##    browser()
##    i <- 1:2000
        ## lines(i,aoiCos1(X$t[i],latitude,longitude,slope=0,pAzimuth))
    x$sunZenith[aoiToDeg(x$sunElevation) < 5] <- pi/2-aoiToRad(5)
    ##browser()
    ##plot(1/cos(x$sunZenith))
    ##
    x$IbTracking <- Ib/cos(x$sunZenith)
    ## Make the aoiCos for the surface
    x$aoiCosSurf <- aoiCos2(X$t, latitude, longitude,  slope, pAzimuth)
    ## no negative
    ##plot(i,x$aoiCosSurf[i])
    x$aoiCosSurf[ x$aoiCosSurf<0 ] <- 0
    ##
    x$IbSurf <- x$IbTracking * x$aoiCosSurf
    ## Clip at maxOut
    ##x$IbSurf[x$IbSurf>maxOut] <- maxOut
    ## Insert zeroes where the IbSurf is NA, but the Ib is not
    ##x$IbSurf[ is.na(x$IbSurf) & !is.na(Ib) ] <- 0
    ## No negative
    x$IbSurf[x$IbSurf<0] <- 0
    ## Return
    return(x$IbSurf)
  }
