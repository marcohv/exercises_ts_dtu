TiTe <- function(Dat)
  {
    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add a system equation and thereby also a state
    model$addSystem(dTi ~ ( 1/(Ci*Rie)*(Te-Ti) + Aw/Ci*Ps + 1/Ci*Ph )*dt + exp(p11)*dw1)
    model$addSystem(dTe ~ ( 1/(Ce*Rie)*(Ti-Te) + 1/(Ce*Rea)*(Ta-Te) )*dt + exp(p22)*dw2)
    ## Set the names of the inputs
    model$addInput(Ta,Ps,Ph)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ti)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))
    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ti = c(init=15  ,lower=0     ,upper=25) )
    model$setParameter(  Te = c(init=10  ,lb=-25   ,ub=25) )
    ## Set the initial value for the optimization
    model$setParameter(  Ci = c(init=5   ,lb=0.1   ,ub=20) )
    model$setParameter(  Ce = c(init=0.5 ,lb=0.1   ,ub=20) )
    model$setParameter( Rie = c(init=5   ,lb=0.1   ,ub=50) )
    model$setParameter( Rea = c(init=0.5 ,lb=0.1   ,ub=50) )
    model$setParameter(  Aw = c(init=10  ,lb=0.1   ,ub=40) )
    model$setParameter( p11 = c(init=0   ,lb=-50   ,ub=10) )
    model$setParameter( p22 = c(init=-1  ,lb=-50   ,ub=10) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10) )
    
    ## Run the parameter optimization
    fit <- model$estimate(Dat)
    ## Replace the data to have all series available for analysis
    fit$data[[1]] <- Dat
    ## Return the fit
    return(fit)
  }
