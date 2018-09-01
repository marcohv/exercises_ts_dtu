Ti <- function(X)
  {
    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add a system equation and thereby also a state
    model$addSystem(dTi ~ ( 1/(Ci*Ria)*(Ta-Ti) + Aw/Ci*Ps + 1/Ci*Ph )*dt + exp(p11)*dw1)
    ## Set the names of the inputs
    model$addInput(Ta,Ps,Ph)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ti)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))
    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ti = c(init=15  ,lb=0     ,ub=25 ) )
    ## Set the initial value for the optimization
    model$setParameter(  Ci = c(init=1   ,lb=1E-5  ,ub=20 ) )
    model$setParameter( Ria = c(init=20  ,lb=1     ,ub=1E4) )
    model$setParameter(  Aw = c(init=20  ,lb=1     ,ub=300) )
    model$setParameter( p11 = c(init=1   ,lb=-30   ,ub=10 ) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10 ) )
    ## Run the parameter optimization
    fit <- model$estimate(X)
    ## Replace the data to have all series available for analysis
    fit$data[[1]] <- X
    ## Return the model, the fit, and the data
    return(fit)
  }
