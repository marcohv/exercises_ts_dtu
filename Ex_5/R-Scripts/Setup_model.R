if(Air){
  cBd <- Bd[,Controllable,drop=FALSE]
  ncBd <- Bd[,!Controllable,drop=FALSE]
}else{
  cBd <- altBd[,Controllable,drop=FALSE]
  ncBd <- altBd[,!Controllable,drop=FALSE]
}

# Penalty <- read.csv("Penalty60min.csv",header = FALSE)$V1[1:Tmax]
# Penalty <- approx(Penalty,n=Tmax+24,method='constant')$y
# 
# Spot <- arima.sim(model=list(ar=c(0.8),ma=c(0.7,0.5)),n=Tmax)+15
# Spot <- approx(Spot,n=Tmax+24,method='constant')$y
# 
# Penalty <- Penalty*15
# PenSpot <- Spot + Penalty



Phie <- NULL
Phix <- NULL
Gu <- NULL
Gd <- NULL
for (i in 1:N){
  Phix <- rbind(Phix, C %*% (Ad %^% i) )
  #Phie <- rbind(Phie, C %*% (Ad %^% (i-1)) %*% K)
  
  gu <- NULL
  gd <- NULL
  for (j in i:(-N+i+1)){
    if (j>0){
      gu <- cbind(gu, C %*% (Ad %^% (j-1)) %*% cBd)
      gd <- cbind(gd, C %*% (Ad %^% (j-1)) %*% ncBd)
    } else{
      gu <- cbind(gu, C %*% (Ad %*% cBd * 0))
      gd <- cbind(gd, C %*% (Ad %*% ncBd * 0))
    }
    
  }
  Gu <- rbind(Gu,gu)
  Gd <- rbind(Gd,gd)
}


Ainq <- cbind(rbind(-Gu,Gu,matrix(0,nrow=dim(Gu)[1],ncol=dim(Gu)[2])),rbind(-diag(dim(Gu)[1]),-diag(dim(Gu)[1]),-diag(dim(Gu)[1])))
dir <- rep("<=",N*3)
types <- rep("C",N*2)


### Comment out if using RGLP 
Ainq <- rbind(Ainq,cbind(diag(N),matrix(0,N,N)))
######

Bounds <- list(lower= list (ind = seq(1L,2*N),val=numeric(2*N)),upper = list(ind=seq(1L,2*N),val=c(rep(MaxHeat,N),rep(Inf,N))))


Sigma <- Noise*matrix(c(exp(fit1$xm[8])/fit1$xm[4]*sqrt(dt),0,
                        0,exp(fit1$xm[9])/fit1$xm[5]*sqrt(dt)),byrow = TRUE,nrow=2)^2

Uncertainty <- sqrt(rowSums(apply(Phix%*%Sigma,2,cumsum)))


ControlBuilding <- function(Price,e,T,Tmax,Tmin,Stochastic=0){
  Penalty <- 1E+12 # Penalty for violating temperature constraints
  N <- length(Price) # Predictin Horizon
  f <- t(c(Price,rep(Penalty,N)))
  
  binq <- matrix(c(-Tmin+qnorm(ViolationFraction)*Stochastic*Uncertainty+Phix%*%T+Gd%*%e,Tmax-qnorm(ViolationFraction)*Stochastic*Uncertainty-Phix%*%T-Gd%*%e,numeric(N),rep(MaxHeat,N)),ncol=1)
  #Rglpk_solve_LP(f,Ainq,dir,binq,bounds=Bounds,types=types) # Change the remaining lines for this one if using LGPK solver
  lp(objective.in = f,const.mat = Ainq,const.dir = "<=",const.rhs = binq)
}
