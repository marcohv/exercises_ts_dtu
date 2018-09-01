aoiToRad <- function(angle)
  {
    angle/180 * pi
  }                                   

aoiToDeg <- function(rad)
  {
    rad/pi * 180
  }

aoiEquationOfTime <- function(time)
{
  day <- as.POSIXlt(time)$yday
  # Day is the number of days since the start of the year
  b <- aoiToRad((360/365) * (day-81))
  # Equation of time in minutes
  9.87 * sin(2*b) - 7.53 * cos(b) - 1.5 * sin(b)
}

aoiLocalSolarTime <- function(time, longitude)
{
  ## time is in UTC
  ## longitude is in degrees. Positive east of greenwich. The earth rotates 1 degrees in 4 minutes.
  time + ( 4 * aoiToDeg(longitude) + aoiEquationOfTime(time) ) * 60
}

aoiSunHourAngle <- function(time, longitude)
{
  ## localSolarTime is in seconds since 1970 given as an POSIXct object
  t <- as.POSIXlt(aoiLocalSolarTime(time, longitude))
  LST.tod <- t$hour + t$min/60 + t$sec/3600
  ## Output hourAngle in rad
  aoiToRad( 15 * (LST.tod  - 12) )
}

aoiSunDeclination <- function(time)
  {
    #### Calculate declination angle
    day <- as.POSIXlt(time)$yday + 1
    ## Return the result in rad
    asin( sin(aoiToRad(23.45)) * sin(aoiToRad((360/365)*(day-81))) )
  }

aoiSunElevation <- function(latitude, hourAngle, declination)
  {
    asin( cos(hourAngle) * cos(declination) * cos(latitude)  + sin(declination) * sin(latitude) )
  }

aoiSunElevationDeg <- function(time, latitude, longitude)
  {
    ## All input angles are given in degrees, transform them into rad
    latitude <- aoiToRad(latitude)
    longitude <- aoiToRad(longitude)
    ## Calculate the earth declination
    declination <- aoiSunDeclination(time)
    ## Calculate the hourAngle
    hourAngle <- aoiSunHourAngle(time, longitude)
    ## Calculate the elevation angle of the sun
    aoiSunElevation(latitude, hourAngle, declination)
  }

aoiSunAzimuth <- function(latitude, elevation, declination, hourAngle)
  {
    ## Works only for latitudes above the max declination of the earth: 23.45 degrees
    if(abs(aoiToDeg(latitude))<=23.45){ stop("Works only for latitudes above the max declination of the earth: 23.45 degrees") }
    ##
    sAzimuth <- acos( (sin(declination) * cos(latitude) - cos(declination) * sin(latitude) * cos(hourAngle))/( cos(elevation) ) )
    i <- hourAngle > 0
    hourAngle[i] <- { sAzimuth[i] <- 2*pi - sAzimuth[i] }
    sAzimuth
  }

aoiSunAzimuthDeg <- function(time, latitude, longitude)
  {
    if(latitude<=23.45){ stop("Works only for latitudes above the max declination of the earth: 23.45 degrees") }
    ## All input angles are given in degrees, transform them into rad
    latitude <- aoiToRad(latitude)
    longitude <- aoiToRad(longitude)
    ## Calculate the earth declination
    declination <- aoiSunDeclination(time)
    ## Calculate the hourAngle
    hourAngle <- aoiSunHourAngle(time, longitude)
    ## Calculate the elevation angle of the sun
    elevation <- aoiSunElevation(latitude, hourAngle, declination)
    ## Return in Rad
    aoiSunAzimuth(latitude, elevation, declination, hourAngle)
  }

aoiCos1 <- function(time, latitude, longitude, slope, pAzimuth)
{
  ## All angle are given in degrees.
  ## slope: is the angle between the normal to the ground surface, and the normal of the panel.
  ## pAzimuth: azimuth of the panel where 0 degrees is due south. + is toward west, - is toward east.
  
  ## All input angles are given in degrees, transform them into rad
  longitude <- aoiToRad(longitude)
  latitude <- aoiToRad(latitude)
  slope <- aoiToRad(slope)
  pAzimuth <- aoiToRad(pAzimuth)

  ## Calculate the earth declination
  dcl <- aoiSunDeclination(time)

  ## Calculate the hourAngle
  hourAngle <- aoiSunHourAngle(time, longitude)

  ## Calculate the angle of incidence
  cosAOI <- (sin(dcl) * sin(latitude) * cos(slope) - sin(dcl) * cos(latitude) * sin(slope) * cos(pAzimuth)
               + cos(dcl) * cos(latitude) * cos(slope) * cos(hourAngle)
               + cos(dcl) * sin(latitude) * sin(slope) * cos(pAzimuth) * cos(hourAngle)
               + cos(dcl) * sin(slope) * sin(pAzimuth) * sin(hourAngle))
  
  ## Return the result 
  return(cosAOI)
}

aoiCos2 <- function(time, latitude, longitude, slope, pAzimuth)
  {
    ## All angle are given in degrees.
    ## slope: is the angle between the normal to the ground surface, and the normal of the panel.
    ## pAzimuth: azimuth of the panel where 0 degrees is due south. + is toward west, - is toward east.
    pAzimuth <- pAzimuth + 180 # The algorithm uses a panel azimuth where 180 degree is due south
    
    ## All input angles are given in degrees, transform them into rad
    longitude <- aoiToRad(longitude)
    latitude <- aoiToRad(latitude)
    slope <- aoiToRad(slope)
    pAzimuth <- aoiToRad(pAzimuth)

    ## Calculate the earth declination
    declination <- aoiSunDeclination(time)

    ## Calculate the hourAngle
    hourAngle <- aoiSunHourAngle(time, longitude)

    ## Calculate the zenith angle of the sun
    elevation <- aoiSunElevation(latitude, hourAngle, declination)
    zenith <- (pi/2) - elevation

    ## Calculate the azimuth of the sun
    sAzimuth <- aoiSunAzimuth(latitude, elevation, declination, hourAngle)

    ## Calculate the angle of incidence
    cosAOI <-  cos(slope) * cos(zenith) + sin(slope) * sin(zenith) * cos(sAzimuth - pAzimuth)

    ## Return the result
    return(cosAOI)
  }



## ###########################################################################
## ## Tests and examples
## ## All times must be in GMT
## t <- seq(ISOdate(2009,6,1,0),ISOdate(2009,6,3,0),by=60)

## ## This will calculate the elevation at the solar collectors at Byg in radians
## latitude <- 25
## longitude <- 12
## ## latitude <- 37
## ## longitude <- -2

## ## Solar elevation
## elevRad <- aoiSunElevation( aoiToRad(latitude), aoiSunHourAngle(t,aoiToRad(longitude)), aoiSunDeclination(t))
## elevRad2 <- aoiSunElevationDeg(t,latitude,longitude)
## ##
## plot(aoiSunAzimuthDeg(t,latitude,longitude),aoiToDeg(elevRad))

## lines(t,aoiToDeg(elevRad2),col=2)

## ## Angle of incidence of some surface
## par(mfrow=c(2,1))
## plot(t,aoiCos1(t, longitude, latitude, slope=90, pAzimuth=0))
