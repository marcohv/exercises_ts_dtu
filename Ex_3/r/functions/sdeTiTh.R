sdeTiTh <- function(X, rerun = FALSE){
    ## Try if the fit was cached
    cache_val <- cache_load(!rerun & !pg$rerun, X, pg$cache_dir)
    if(class(cache_val) == "ctsmr"){ return(cache_val) }

    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add a system equation and thereby also a state
    model$addSystem(dTi ~ ( 1/(Ci*Rih)*(Th-Ti) + 1/(Ci*Rie)*(Te-Ti) + Aw/Ci*Gv  )*dt + exp(p11)*dw1 )
    model$addSystem(dTh ~ ( 1/(Ch*Rih)*(Ti-Th) + 1/Ch*Qi)*dt + exp(p22)*dw2 )
    ## Set the names of the inputs
    model$addInput(Te,Gv,Qi)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ti)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))

    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ti = c(init=35  ,lb=20    ,ub=45) )
    model$setParameter(  Th = c(init=40  ,lb=20    ,ub=200) )
    ## Set the initial value for the optimization
    model$setParameter(  Ci = c(init=1E5 ,lb=1E4   ,ub=1E7) )
    model$setParameter(  Ch = c(init=1E4 ,lb=1E2   ,ub=1E6) )
    model$setParameter( Rie = c(init=0.2 ,lb=0.01  ,ub=2 ) )
    model$setParameter( Rih = c(init=0.01,lb=1E-5  ,ub=2) )
    model$setParameter(  Aw = c(init=0.1 ,lb=0.001 ,ub=10))
    model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( p22 = c(init=1   ,lb=-50   ,ub=10) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )
    
    ## Run the parameter optimization
    fit <- model$estimate(data = X, firstorder=pg$firstorder, threads = pg$threads)
    fit$data[[1]] <- X
    fit$Rnames <- c("Rie")

    ## Save the fit, filename is generated above and kept in pg$cache_dir folder
    cache_save(fit, pg$cache_dir, cache_val)

    ## Return the fit
    return(fit)
}
