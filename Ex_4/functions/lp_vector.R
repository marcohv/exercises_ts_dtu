lp_vector<- function(x, a1) {
    ## Make a 1'st order low pass filter as (5.3) p.46 in the HAN report.
    y <- numeric(length(x))
    ## First value in x is the init value
    y[1] <- x[1]
    ## 
    for (i in 2:length(x)) {
        if (is.na(y[i - 1])) {
            y[i] <- x[i]
        } else {
            y[i] <- a1 * y[i - 1] + (1 - a1) * x[i]
        }
    }
    ## Return (afterwards the init value y[1], must be handled)
    return(y)
}

## cppFunction("NumericVector lp_vector_cpp(NumericVector x, double a1) {
##   int n = x.size();
##   NumericVector y(n);
##   double oma1 = (1-a1);

##   // First value in x is the init value of y
##   y[0] = x[0];

##   for(int i = 1; i < n; ++i) {
##     if(NumericVector::is_na(y[i-1])){
##       y[i] = x[i];
##     }else{
##       y[i] = a1*y[i-1] + oma1*x[i];
##     } 
##   }
##   // Return (afterwards the init value (i.e. first value in y[0]), must be handled)
##   return y;
## }")

## ## Test ##x <- c(rep(0,10),rep(1,10)) x <- rnorm(200) x[5] <- NA lpVec(x, 0.8)
## lpVecC(x, 0.8)

## plot(x) lines(lpVecC(x, 0.8))

## require(microbenchmark) microbenchmark( lpVec(x, 0.8), lpVecC(x, 0.8) )
