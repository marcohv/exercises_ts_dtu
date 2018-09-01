rls <- function(frml, lambda, data, k) {
    
    ## R build-in function for setting up linear regression model
    mf <- model.frame(as.formula(frml), data, na.action = na.pass)
    ## The model output
    y <- mf[ ,1]
    ## The design matrix
    X <- model.matrix(as.formula(frml), mf)

    ## The number of observations
    n <- nrow(X)
    ## The number of parameters
    p <- ncol(X)
    ## Parameter matrix
    Theta <- matrix(as.numeric(NA), nrow = n, ncol = p)
    ## The predictions
    yhat <- as.numeric(rep(NA,n))  
    ## The parameter vector
    theta <- matrix(rep(0,p), ncol = 1)
    
    ## Start value for the parameter covariance P
    P <- 10000 * diag(1, p)
    ## Use the inverse in the RLS
    R <- solve(P)
    
    ## Iterate through and estimate the parameters
    for (i in 1:(n-k)) {
        x <- matrix(X[i, ])
        ## Check for NAs in inputs and output
        if(all(!is.na(x)) & !is.na(y[i])){
            ## Update
            R <- lambda * R + x %*% t(x)
            theta <- theta + solve(R, x) %*% (y[i] - t(x) %*% theta)
            Theta[i, ] <- t(theta)
        }
        ## Predict
        x <- matrix(X[i+k, ])
        if(all(!is.na(x))){
            yhat[i+k] <- t(x) %*% theta
        }
    }
    
    ## Return a list
    L <- list()
    L$residuals <- y - yhat
    L$X <- X
    L$y <- y
    L$Theta <- Theta
    return(L)
}
