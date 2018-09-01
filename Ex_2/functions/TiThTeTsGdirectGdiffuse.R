TiThTeTsGdirectGdiffuse <- function(Dat)
  {
    ## Generate a new object of class ctsm
    model <- ctsm$new()
    ## Add a system equation and thereby also a state
    model$addSystem(dTs ~ ( 1/(Cs*Ris)*(Ti-Ts) )*dt + exp(p11)*dw1 )
    model$addSystem(dTi ~ ( 1/(Ci*Ris)*(Ts-Ti) + 1/(Ci*Rih)*(Th-Ti) + 1/(Ci*Rie)*(Te-Ti) + Awdirect/Ci*Gdirect + Awdiffuse/Ci*Gdiffuse )*dt + exp(p22)*dw2 )
    model$addSystem(dTh ~ ( 1/(Ch*Rih)*(Ti-Th) + 1/Ch*Ph)*dt + exp(p33)*dw3 )
    model$addSystem(dTe ~ ( 1/(Ce*Rie)*(Ti-Te) + 1/(Ce*Rea)*(Ta-Te) )*dt + exp(p44)*dw4 )
    ## Set the names of the inputs
    model$addInput(Ta,Ph,Gdirect)
    ## Set the observation equation: Ti is the state, yTi is the measured output
    model$addObs(yTi ~ Ts)
    ## Set the variance of the measurement error
    model$setVariance(yTi ~ exp(e11))
    ## Set the initial value (for the optimization) of the value of the state at the starting time point
    model$setParameter(  Ts = c(init=20  ,lb=0     ,ub=25) )
    model$setParameter(  Ti = c(init=20  ,lb=0     ,ub=25) )
    model$setParameter(  Th = c(init=20  ,lb=0     ,ub=25) )
    model$setParameter(  Te = c(init=10  ,lb=0     ,ub=25) )
    ## Set the initial value for the optimization
    model$setParameter(  Cs = c(init=0.1 ,lb=1E-5  ,ub=20) )
    model$setParameter(  Ci = c(init=1   ,lb=1E-3  ,ub=25) )
    model$setParameter(  Ch = c(init=1   ,lb=1E-5  ,ub=10) )
    model$setParameter(  Ce = c(init=1   ,lb=1E-2  ,ub=300))
    model$setParameter( Ris = c(init=1   ,lb=1E-5  ,ub=50) )
    model$setParameter( Rea = c(init=1   ,lb=1E-2  ,ub=1E3))
    model$setParameter( Rih = c(init=1   ,lb=1E-3  ,ub=100))
    model$setParameter( Rie = c(init=10  ,lb=1E-1  ,ub=100))
    model$setParameter( Awdirect = c(init=20  ,lb=0.01  ,ub=300))
    model$setParameter( Awdiffuse = c(init=5  ,lb=0.01  ,ub=300))
    model$setParameter( p11 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( p22 = c(init=-1  ,lb=-50   ,ub=20) )
    model$setParameter( p33 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( p44 = c(init=1   ,lb=-50   ,ub=20) )
    model$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=20) )
    
    ## Run the parameter optimization
    fit <- model$estimate(Dat)
    ## Replace the data to have all series available for analysis
    fit$data[[1]] <- Dat
    ## Return the fit
    return(fit)
  }
