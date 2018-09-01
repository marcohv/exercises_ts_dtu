sdeTiThTw2 <- function(X, rerun = FALSE){
    ## Try if the fit was cached
    cache_val <- cache_load(!rerun & !pg$rerun, X, pg$cache_dir)
    if(class(cache_val) == "ctsmr"){ return(cache_val) }

    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add system equations and thereby also states        
    model$addSystem(dTi  ~ ( 1/(Ci*Riw)*(Tw1-Ti) + p*Aw/Ci*Gv + 1/(Ci*Rih)*(Th-Ti) )*dt + exp(p11)*dw1)
    model$addSystem(dTh  ~ ( 1/(Ch*Rih)*(Ti-Th) + 1/Ch*Qi )*dt + exp(p22)*dw2)
    model$addSystem(dTw1 ~ ( 1/(Cw1*Riw)*(Ti-Tw1) + 1/(Cw1*Rww)*(Tw2-Tw1) + (1-p)*Aw/Cw1*Gv )*dt + exp(p33)*dw3) 
    model$addSystem(dTw2 ~ ( 1/(Cw2*Rww)*(Tw1-Tw2) + 1/(Cw2*Rwe)*(Te-Tw2) )*dt + exp(p44)*dw4)
    ## Set the names of the inputs
    model$addInput(Te,Gv,Qi)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ti)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))

    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ti  = c(init=35  ,lb=20    ,ub=45) )
    model$setParameter(  Th  = c(init=75 ,lb=20    ,ub=600))
    model$setParameter(  Tw1 = c(init=30  ,lb=20    ,ub=45) )
    model$setParameter(  Tw2 = c(init=25  ,lb=20    ,ub=45) )
    ## Set the initial value of the parameters for the optimization
    model$setParameter(  Ci  = c(init=1E4 ,lb=1E1   ,ub=1E7) )
    model$setParameter(  Ch  = c(init=1E4 ,lb=1E1   ,ub=1E6) )
    model$setParameter(  Cw1 = c(init=2E5 ,lb=1E4   ,ub=1E8) )
    model$setParameter(  Cw2 = c(init=2E5 ,lb=1E4   ,ub=1E8) )
    model$setParameter( Riw = c(init=0.1  ,lb=1E-5  ,ub=1) )
    model$setParameter( Rih = c(init=0.01 ,lb=1E-5  ,ub=1) )
    model$setParameter( Rww = c(init=0.1 ,lb=1E-2  ,ub=1) )
    model$setParameter( Rwe = c(init=0.2 ,lb=1E-2  ,ub=1) )
    model$setParameter(  Aw = c(init=0.1 ,lb=0.01 ,ub=1) )
    model$setParameter(   p = c(init=0.1 ,lb=0     ,ub=1 ) )
    model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( p33 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( p44 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )

    ## Run the parameter optimization
    fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)
    fit$data[[1]] <- X
    fit$Rnames <- c("Rwe","Riw","Rww")
    
    ## Save the fit, filename is generated above and kept in pg$cache_dir folder
    cache_save(fit, pg$cache_dir, cache_val)
    
    ## Return
    return(fit)
}
