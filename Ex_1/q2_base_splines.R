## ----Initialize-----------------------------------------------
## Remove all in memory
rm(ls = list())

## Load the splines package (it is included in base R)
require(splines)

## Now the bs function is available
?bs
## See what it returns for a sequence from -1 to 1
x = seq(-1,1,len=1000)
x_bs <- bs(x, intercept = TRUE)
x_bs
str(x_bs)
## It is actually a matrix with attributes
class(x_bs[ , ])
## Use colnames() for matrix instead of names() for data.frame
colnames(x_bs[ , ])

## Make a function for plotting
plot_bs <- function(x, x_bs){
    ## Merge them with x
    X <- data.frame(x, x_bs)
    names(X) <- c("x",paste0("bs",1:(ncol(X)-1)))
    ## Plot
    icol <- grep("^bs", names(X))
    plot(X$x, X$bs1, type="n", ylim=range(X[ ,icol]))
    for(i in icol){
        lines(X$x, X[ ,i], col=i)
    }
}
plot_bs(x, x_bs)



## ----Degrees of freedom-----------------------------------------
## Change degrees of freedom (of the spline function)
df <- 7
## Generate the base splines
x_bs <- bs(x, df = df, intercept = TRUE)[ , ]
## Plot and add the quantiles
plot_bs(x, x_bs)

## Change degree (of the piece-wise polynomials, i.e. polynomials between the knot points)
df <- 7
degree <- 1
## Generate the base splines
x_bs <- bs(x, df = df, degree = degree, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)



## ----Specify knot points----------------------------------------
## Or specify the knot points directly
## As the quantiles
df <- 7
knots <- quantile(x, probs=seq(0, 1, by=1/(df-1)))
knots

## Generate the base splines of degree = 1 with the knots as the quantiles
##
## Note that the inner knots are given by the "knots" argument, the
##   boundary knots are min(x) and max(x) (see ?bs "Boundary.knots")
x_bs <- bs(x, knots = knots[2:(length(knots)-1)], degree = 1, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
abline(v = knots)

## Another degree
degree <- 3

## Generate the base splines of degree with the quantile knots
x_bs <- bs(x, knots = knots[2:(length(knots)-1)], degree = degree, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
abline(v = knots)



## ----Non-equidistant quantiles--------------------------------
## Another sequence, where the quantiles are not equidistant
x = c(seq(-1,0,len=200), seq(0,1,len=100), seq(1,2,len=500))

## Set df and degree
df <- 7
degree <- 1

## Generate the base splines of degree with the quantile knots
x_bs <- bs(x, df = df, degree = degree, intercept = TRUE)[ , ]
## Plot
plot_bs(x, x_bs)
abline(v = knots)