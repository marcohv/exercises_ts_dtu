prbs <- function(n, initReg=666, lambda=1)
  {
    ## Function for generating PRBS sequences.
    ## - n is the length of the register
    ## - initReg just needs to be some initial value of 1,2,...
    ##   it is the initial value of the registers and therefore only
    ##   determines the start of the cycle
    ## - lambda is the length of the smallest period in
    ##   which the signal can change, given in samples
    ##
    ## Check the input
    if(n < 2 | n > 11){ stop("n must be between 1 and 11") }
    ## Do init
    reg <- intToBits(as.integer(initReg))
    print(reg)
    x <- vector()
    N <- 2^n - 1
    ## Do the shift according to the value of n
    for(i in 1:N)
      {
        ## Make the xor operation according to Godfrey80
        if(n <= 4 | n == 6){ reg[n+1] <- xor(reg[1],reg[2]) }
        else if(n == 5 | n == 11){ reg[n+1] <- xor(reg[1],reg[3]) }
        else if(n == 7 | n == 10){ reg[n+1] <- xor(reg[1],reg[4]) }
        else if(n == 8){ reg[n+1] <- as.raw(sum(as.integer(c(reg[1],reg[5],reg[6],reg[7])))%%2) }
        else if(n == 9){ reg[n+1] <- xor(reg[1],reg[5]) }
        ## Keep the value of the first position in the register
        x[i] <- as.integer(reg[1])
        ## Shift the register
        reg[1:n] <- reg[2:(n+1)]
      }
    ## Return x with each element repeated lambda times
    rep(x, rep(lambda,N))
  }
