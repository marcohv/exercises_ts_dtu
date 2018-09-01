sdeTiTh2Tw <- function(X, rerun = FALSE){
    ## Try if the fit was cached
    cache_val <- cache_load(!rerun & !pg$rerun, X, pg$cache_dir)
    if(class(cache_val) == "ctsmr"){ return(cache_val) }

    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add a system equation and thereby also a state
    model$addSystem(dTi ~ ( 1/(Ci*Rih)*(Th1-Ti) + 1/(Ci*Riw)*(Tw-Ti) + Aw/Ci*Gv  )*dt + exp(p11)*dw1 )
    model$addSystem(dTh1 ~ ( 1/(Ch1*Rih)*(Ti-Th1) + 1/(Ch1*Rhh)*(Th2-Th1))*dt + exp(p22)*dw2 )
    model$addSystem(dTh2 ~ ( 1/(Ch2*Rhh)*(Th1-Th2) + 1/Ch2*Qi)*dt + exp(p33)*dw3 )
    model$addSystem(dTw ~ ( 1/(Cw*Riw)*(Ti-Tw) + 1/(Cw*Rwe)*(Te-Tw) )*dt + exp(p44)*dw4 )
    ## Set the names of the inputs
    model$addInput(Te,Gv,Qi)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ti)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))
    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ti = c(init=35  ,lb=20    ,ub=45) )
    model$setParameter(  Th1 = c(init=100,lb=20   ,ub=400) )
    model$setParameter(  Th2 = c(init=100,lb=20   ,ub=400) )
    model$setParameter(  Tw = c(init=34  ,lb=20    ,ub=45) )
    ## Set the initial value for the optimization
    model$setParameter(  Ci = c(init=2E4 ,lb=1E4   ,ub=1E7) )
    model$setParameter(  Ch1= c(init=1E4 ,lb=1     ,ub=1E5) )
    model$setParameter(  Ch2= c(init=1E4 ,lb=1     ,ub=1E5) )
    model$setParameter(  Cw = c(init=3E5 ,lb=1E4   ,ub=1E8) )
    model$setParameter( Rih = c(init=0.01,lb=1E-6  ,ub=10))
    model$setParameter( Rhh = c(init=0.01,lb=1E-6  ,ub=10))
    model$setParameter( Riw = c(init=0.01,lb=1E-5  ,ub=10) )
    model$setParameter( Rwe = c(init=0.3 ,lb=1E-5  ,ub=10) )
    model$setParameter(  Aw = c(init=0.1 ,lb=0.001 ,ub=10))
    model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( p33 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( p44 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=20) )
    
    ## Run the parameter optimization
    fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)
    fit$data[[1]] <- X
    fit$Rnames <- c("Rwe","Riw")

    ## Save the fit, filename is generated above and kept in pg$cache_dir folder
    cache_save(fit, pg$cache_dir, cache_val)

    ## Return
    return(fit)
}
