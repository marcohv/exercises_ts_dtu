prepare_data <- function(X, tunit = "secs", exclude = 0, samples_after_Qi_step = 0, rmvTs = 0, solarOuterSurf = "Gvn.2"){
    X$timedate <- X$t
    X$t <- as.numeric(X$t-X$t[1],units=tunit)
    ##
    X$yTi <- X$Ti
    ## The solar radiation entering the outer surface
    X$Gouter <- X[,solarOuterSurf]
    ## Exclude after switching state
    if(exclude!=0)
    {
        ## 
        i <- which(abs(diff(X$Qi))>10)
        X$yTiOr <- X$yTi
        X$yTi[unique(c(sapply(i,function(x){x:(x+exclude)})))] <- NA
    }
    ## Generate a signal with another level after switching
    if(samples_after_Qi_step != 0)
    {
        ## swithcing states
        i <- which(abs(diff(X$Qi))>10)
        X$stepQi <- 0
        L <- unique(c(sapply(i,function(x){(x-1):(x+samples_after_Qi_step)})))
        if(length(L)>0)
        {
            X$stepQi[L] <- 1
            ## First part also high level
            X$stepQi[1:min(which(X$stepQi==1))] <- 1
        }
    }
    ## Only keep output values with a given interval
    if(rmvTs!=0)
    {
        ##browser()
        yTi <- X$yTi
        i <- diff(floor(cumsum(asSeconds(diff(X$timedate))/60/rmvTs)))==1
        X$yTi <- NA
        X$yTi[1] <- yTi[1]
        X$yTi[i] <- yTi[i]
    }
    ##
    return(X)
}
