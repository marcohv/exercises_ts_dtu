readSeries <- function(nameSeries,Ts,nlags=8,splMake=FALSE,splNKnots=2,splDegree=2,splName="",newdev=TRUE,plotsplines=TRUE,rmvHours=8)
  {
    ## Read data, X
    load(paste0("../data/",nameSeries,".rda"))

    ## Resample
    X <- resampleDF(X,Ts*60,startTime=X$t[1])
    X$tod <- asHours(X$t - trunc(X$t,"day"))

    ## Make signals
    if(length(grep("^Ti",names(X)))>0){ X$Ti <- (X$Ti.down+X$Ti.up)/2 }

    if(!("Te"%in%names(X))) X$Te <- (X$Te.down+X$Te.middle)/2

    if(length(grep("^Ti",names(X)))>0){ X$deltaT <- X$Ti - X$Te }

    ## Calc the sun azimuth
    if(length(grep("^X",nameSeries))>0){latitude <- 37.097083; longitude <- -2.364994}
    if(length(grep("^ce3",nameSeries))>0){latitude <- 50.683333; longitude <- 4.516667}
    ##
    X$sunAzimuth <- aoiSunAzimuthDeg(X$t-diff(X$t[1:2])/2, latitude, longitude)
    X$sunElevation <- aoiSunElevationDeg(X$t,latitude,longitude)

    ## Make lagged versions of the inputs
    if(!is.na(nlags))
      {
        X[,pst("Qi.l",0:nlags)] <- lagMat(X$Qi, lag=0:nlags)
        X[,pst("Ti.l",0:nlags)] <- lagMat(X$Ti, lag=0:nlags)
        ##
        if(length(grep("^Ti",names(X)))>0){ X[,pst("Ti.l",0:nlags)] <- lagMat(X$Ti, lag=0:nlags) }
        ##
        X[,pst("Te.l",0:nlags)] <- lagMat(X$Te, lag=0:nlags)
        ##
        if(length(grep("^Ti",names(X)))>0){ X[,pst("deltaT.l",0:nlags)] <- lagMat(X$Ti-X$Te, lag=0:nlags) }
        ## Solar radiation
        X[,pst("Gv.l",0:nlags)] <- lagMat(X$Gv, lag=0:nlags)
        ##
        if("Gh.1"%in%names(X)) X[,pst("Gh.1.l",0:nlags)] <- lagMat(X$Gh.1, lag=0:nlags)
        ##
        if("Gh.dif.1"%in%names(X)) X[,pst("Gh.dif.1.l",0:nlags)] <- lagMat(X$Gh.dif.1, lag=0:nlags)
        ##
        if("Gh.1"%in%names(X)) X[,pst("Gh.bea.1.l",0:nlags)] <- lagMat(X$Gh.1-X$Gh.dif.1, lag=0:nlags)
        ##
        if("Gh"%in%names(X) & "Gh.dif"%in%names(X)) X[,pst("Gh.b.l",0:nlags)] <- lagMat(X$Gh-X$Gh.dif, lag=0:nlags)
        ##
        if("Gh.dif"%in%names(X)) X[,pst("Gh.dif.l",0:nlags)] <- lagMat(X$Gh.dif, lag=0:nlags)
        ##
        if("WV"%in%names(X)) X[,pst("WV.l",0:nlags)] <- lagMat(X$WV, lag=0:nlags)
        ##
        if("WV"%in%names(X)&"deltaT"%in%names(X)) X[,pst("WVDeltaT.l",0:nlags)] <- lagMat(X$WV*X$deltaT, lag=0:nlags)
        ##
        if("Glw.h.2"%in%names(X)) X[,pst("Glw.h.2.l",0:nlags)] <- lagMat(X$Glw.h.2, lag=0:nlags)
        if("Glw.v.1"%in%names(X)) X[,pst("Glw.v.1.l",0:nlags)] <- lagMat(X$Glw.v.1, lag=0:nlags)
        if("Gvn.2"%in%names(X)) X[,pst("Gvn.2.l",0:nlags)] <- lagMat(X$Gvn.2, lag=0:nlags)
                                        #        X[,pst("GvE.l",0:nlags)] <- lagMat(X$GvE.b+X$Gvp.dif, lag=0:nlags)
                                        #        X[,pst("GvS.l",0:nlags)] <- lagMat(X$GvS.b+X$Gvp.dif, lag=0:nlags)
                                        #        X[,pst("GvW.l",0:nlags)] <- lagMat(X$GvW.b+X$Gvp.dif, lag=0:nlags)
                                        #        X[,pst("GbvE.l",0:nlags)] <- lagMat(X$GvE.b, lag=0:nlags)
                                        #        X[,pst("GbvS.l",0:nlags)] <- lagMat(X$GvS.b, lag=0:nlags)
                                        #        X[,pst("GbvW.l",0:nlags)] <- lagMat(X$GvW.b, lag=0:nlags)
                                        #        X[,pst("GbvSEWh.l",0:nlags)] <- lagMat(X$GvE.b+X$GvS.b+X$GvW.b+X$Gh.b, lag=0:nlags)
                                        #        X[,pst("GdvSEWh.l",0:nlags)] <- lagMat(X$Gvp.dif+X$Gvp.dif+X$Gvp.dif+X$Gh.dif, lag=0:nlags)
                                        #        X[,pst("Gh.l",0:nlags)] <- lagMat(X$Gh, lag=0:nlags)
                                        #        X[,pst("Gbh.l",0:nlags)] <- lagMat(X$Gh.b, lag=0:nlags)
                                        #        X[,pst("Gdv.l",0:nlags)] <- lagMat(X$Gvp.dif, lag=0:nlags)
                                        #        X[,pst("Gdh.l",0:nlags)] <- lagMat(X$Gh.dif, lag=0:nlags)
                                        #        X[,pst("Ws.l",0:nlags)] <- lagMat(X$WV, lag=0:nlags)
                                        #        X[,pst("WsTa.l",0:nlags)] <- lagMat(X$WV*X$Te.l0, lag=0:nlags)
      }                


    ##--------------------------------------------------------------------
    ## Make splines
    if(splMake)
      {
        ## The minmax range of sun azimuth for positive sun elevation
        spllim <- max(abs(pi - range(X$sunAzimuth[X$sunElevation>0])))
        splSunAzimuth <- pi - X$sunAzimuth
        splSunAzimuth[abs(splSunAzimuth)>spllim] <- NA
        ##
        require(splines)
        ## Play with splines
        ##spl <- bs(splSunAzimuth,knots=seq(-spllim,spllim,len=splNKnots+2)[-c(1,splNKnots+2)],degree=splDegree,Boundary.knots=c(-spllim*1.5,spllim*1.5),intercept=TRUE)
        spl <- bs(splSunAzimuth,knots=seq(-spllim,spllim,len=splNKnots+2)[-c(1,splNKnots+2)],degree=splDegree,Boundary.knots=c(-spllim,spllim),intercept=TRUE)
        ## Make the inputs
        ##spl <- spl[,2:(splNKnots+2)]
        spl <- as.data.frame(spl)
        names(spl) <- paste0("spl",1:ncol(spl))
        tmp <- spl * X[,splName]
        tmp[is.na(tmp)] <- 0
        ## ## Plot them
        if(newdev) dev.new()
        if(plotsplines)
          {
            plot(splSunAzimuth,spl[,1],type="n",ylim=range(spl,na.rm=TRUE),ylab="Base splines",xlab="Sun azimuth angle in rad.")
            for(i in 1:ncol(spl)) lines(splSunAzimuth,spl[,i],col=i)
          }
        ## Keep also the splines
        X <- cbind(X,spl)
        ## The splines
        if(!is.na(nlags))
          {
            for(i in 1:ncol(tmp))
              {
                X[,pst("Gspl",i,".l",0:nlags)] <- lagMat(tmp[,paste0("spl",i)], lag=0:nlags)
              }
          }
        else
          {
            for(i in 1:ncol(tmp))
              {
                X[,pst("Gspl",i)] <- tmp[,paste0("spl",i)]
              }
          }
      }
    ## Remove such that the same period is always removed independent of the sample period
    if(!is.na(rmvHours)) X <- X[X$t>(trunc(X$t[1],units="hours")+rmvHours*3600) ,]

    invisible(X)
  }
