# Read libraries and initiate functions -----------------------------------
library(ctsmr)

# install.packages("expm")
library(expm)

## If you have a working GLPK solver on your system and want to implement a computationally more optimized controller use the following
# install.packages("curl")
# install.packages("devtools")
# library(devtools)
# install_version("slam",version="0.1-40", repos = "http://cran.us.r-project.org")
# Now install the Rglpk which is used for solving linear programs
# install.packages("Rglpk")
# library(Rglpk)



# install.packages("scales")
library(scales)

# install.packages("lpSolve")
library(lpSolve)

source("../functions/asHours.R")
source("../functions/asP.R")
source("../functions/prbs.R")




# Fit grey box model ------------------------------------------------------
data <- read.csv("../data/data.csv",stringsAsFactors=FALSE)
## Give names to the series
names(data) <- c("timedate","Y1","Y2","Ta","Gv","Ph1","Ph2")

data$timedate <- asP(data$timedate)
## timedate is the time in POSIXct, make t in hours since begining
data$t <- asHours(data$timedate-data$timedate[1])


model1 <- ctsm()

model1$addSystem(dT1a ~ ( 1/(C1a*R1m)*(T1m-T1a) + 1/(C1a*R1a)*(Ta-T1a) + 1/C1a*Ph1 + p1*A1w/C1a*Gv)*dt + exp(p1a)/C1a*dw1a)
model1$addSystem(dT1m ~ (1/(C1m*R1m)*(T1a-T1m)+(1-p1)*A1w/C1m*Gv)*dt + exp(p1m)/C1m*dw1m)

## Set the names of the inputs
model1$addInput(Ta,Ph1,Gv)
## Set the observation equation: Ti is the state, Yi is the measured output
model1$addObs(Y1 ~ T1a)

## Set the variance of the measurement error
model1$setVariance(Y1 ~ exp(e11))

## Set the initial value (for the optimization) of the value of the state at the starting time point
model1$setParameter(  T1a0 = c(init=25  ,lb=0     ,ub=35 ) )
model1$setParameter(  T1m0 = c(init=25  ,lb=0     ,ub=35 ) )

## Set the initial value for the optimization
model1$setParameter( C1a = c(init=6   ,lb=1E-5  ,ub=20) )
model1$setParameter( C1m = c(init=12   ,lb=1  ,ub=50) )
model1$setParameter( R1a = c(init=10   ,lb=1     ,ub=80) )
model1$setParameter( R1m = c(init=1   ,lb=1E-10     ,ub=10) )
model1$setParameter( A1w = c(init=1   ,lb=1E-10     ,ub=10) )
model1$setParameter( p1 = c(init=0.5   ,lb=0     ,ub=1) )
model1$setParameter( p1a = c(init=1   ,lb=-30   ,ub=10 ) )
model1$setParameter( p1m = c(init=1   ,lb=-30   ,ub=10 ) )
model1$setParameter( e11 = c(init=-1  ,lb=-50   ,ub=10 ) )
## Run the parameter optimization
fit1 <- model1$estimate(data)

Estimated <- list2env(as.list(fit1$xm))

A <- matrix(sapply(fit1$model$sys.eqs$amat,eval,envir = Estimated),nrow=length(fit1$model$states))
B <- matrix(sapply(fit1$model$sys.eqs$bmat,eval,envir = Estimated),nrow=length(fit1$model$states))
C <- matrix(sapply(fit1$model$obs.eqs$cmat,eval,envir = Estimated),nrow=length(fit1$model$outputs))
D <- matrix(sapply(fit1$model$obs.eqs$dmat,eval,envir = Estimated),nrow=length(fit1$model$outputs))



# Change from air heating to floor heating
altB <- B
altB[1,2] <- 0
altB[2,2] <- 1/fit1$xm[5]



# Parameters of continous time model --------------------------------------
A
B    # Air heating 
altB # floor heating
C
D


# Discretize Model --------------------------------------------------------
dt <- diff(data$t)[1] #all.equal(diff(diff(data$t)),rep(0,length(data$t)-2)) ## Make sure that time steps are uniform

Ad <- expm(A*dt)
Bd <- solve(A)%*%(Ad-diag(dim(A)[1]))%*%B
altBd <- solve(A)%*%(Ad-diag(dim(A)[1]))%*%altB
Cd <- C
Dd <- D

# Parameters of Discrete time model ---------------------------------------
Ad
Bd    # Air heating
altBd # Floot heating
Cd
Dd



# Heating data ------------------------------------------------------------
data$Ph1


# Perform Control ---------------------------------------------------------

set.seed(133)

Controllable <- c(FALSE,TRUE,FALSE) # The controllable inputs e.g. heating in this example, but not outside air temperature or solar radiation
MaxHeat <- 1.5 # Maximum amount of heating
N <- 60 # Control horizon
Ymax <- 25 # Maximum temperature
Ymin <- 22.5 # Minimum temperature
Air <- TRUE # air heating or floor heating?
Noise <- 0

Stochastic <- FALSE # Stochastic boundaries?
ViolationFraction <- 0.05 # How many temperature violations?
Tmax <- length(data$timedate)-N
Price <- rep(1,Tmax+N) # Should the prices be constant?
#Price <- prbs(6,322,10)+1 # Or varying?
ForecastedPrice <- Price

source("Setup_model.R")


### Simulate brownian motion

### Simulate measurement errors
Bin <- cumsum(rnorm(Tmax,sd=exp(fit1$xm[8])/fit1$xm[4]*sqrt(dt)))
Bm <- cumsum(rnorm(Tmax,sd=exp(fit1$xm[9])/fit1$xm[5]*sqrt(dt)))

BM <- Noise*rbind(Bin,Bm)

### Simulate the system
Tall <- matrix(0,nrow=2,ncol=Tmax)
u <- numeric(Tmax)


Tall[,1] <- c(23,23)

for(i in 2:Tmax){
  u[i] <- ControlBuilding(c(Price[i],ForecastedPrice[(i+1):(i+N-1)]),c(rbind(data$Ta[i:(i+N-1)],data$Gv[i:(i+N-1)])),Tall[,(i-1)],Ymax,Ymin,Stochastic)$solution[1]
  Tall[,i] <- Ad%*%Tall[,(i-1)]+1*cBd%*%u[i]+ncBd%*%c(data$Ta[i],data$Gv[i])+t(diff(t(BM)))[,(i-1),drop=FALSE]
}

plot(Tall[1,],type='l',ylim=c(Ymin-0.5,Ymax+0.5),ylab="Air Temperature")
lines(Tall[2,],type='s',col=alpha("blue", 0.4))
lines(u*0.5+Ymin+0.5,type='s',col='red')
lines((Price-1)*0.6+Ymin+1.5,col='green')
lines(c(Ymax,Ymax) ~ c(0,Tmax),lty=2)
lines(c(Ymin,Ymin) ~ c(0,Tmax),lty=2)
legend(70,max(Ymax+0.6),c("Air Temperature","Thermal Mass Temperature",'Heating'),col=c('Black',alpha("blue",0.4),'red'),lty=1,bty='n')



