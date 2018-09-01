plotTSBeg <- function(n)
{
    layout(1:(n+2),heights=c(lcm(0.02),rep(1,n),lcm(1)))
    par(mar=c(0,3,0,1), xaxt="n", mgp=c(2,0.6,0), tcl=-0.4)
    plot(0,type="n",xlab="",ylab="",axes=FALSE)
}
