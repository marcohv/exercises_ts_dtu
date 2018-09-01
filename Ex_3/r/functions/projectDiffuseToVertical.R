projectDiffuseToVertical <- function(X,IName,IdName=NA,IbName=NA)
  {
    t <- X$t
    I <- X[,IName]
    if(!is.na(IdName))
        {
            Id <- X[,IdName]
            if(is.na(IbName)) Ib <- I - Id
        }
    if(!is.na(IbName))
        {
            Ib <- X[,IbName]
            if(is.na(IdName)) Id <- I - Ib
        }

    sunElevation <- X$sunElevation
    zenith <- pi/2-sunElevation
    ## Project the diffuse to the vertical plane
    Rb <- tan(zenith)
    ## Extraterristrial radiation
    G0 <- 1367 * (1 + 0.033*cos( 2*pi*as.POSIXlt(t)$yday/365)) * cos(zenith)
    Ai <- Ib / G0
    ## Clip it
    Ai[Ai<0] <- 0; Ai[Ai>1] <- 1
    pg <- 0.2
    ## Do it
    Idv <- Id * Ai * Rb + Id * (1-Ai) * 0.5 + I * pg * 0.5
    ## No negative
    Idv[Idv<0] <- 0
    ## Return it
    return(Idv)
  }
