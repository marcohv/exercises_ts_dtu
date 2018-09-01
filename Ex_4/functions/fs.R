##############################################################
## Function for generating Fourrier series as a function of x E.g. use for
## harmonic functions for modelling the diurnal patterns or for basis functions
## Input: X is dataframe with columns k1,k2,..., must be normalized to a period of 1
##        n_harmonics the number of harmonics Value: List of dataframes two for each harmonic
################################################################ 
fs <- function(X, n_harmonics) {
    do.call("c", lapply(1:n_harmonics, function(i) {
        val <- list(sin(i * X * 2 * pi), cos(i * X * 2 * pi))
        names(val) <- paste0(c("sin_", "cos_"), i)
        return(val)
    }))
}


## Old stuf for tseq as POSIX: ## Make the x <- asSeconds(tSeq) %% (24*3600)
## Harmonics <- data.frame(t=tSeq) tSeq <- as.POSIXlt(tSeq) ## For work days for(i
## in 1:n_harmonics){ ## Sine term y <- sin(i*2*x/(24*3600)*pi) ## Set to NA if
## weekend day. wday has sunday==0 y[tSeq$wday==0 | tSeq$wday==6] <- 0 Harmonics[
## ,paste0('sinWork',i)] <- y ## Cos term y <- cos(i*2*x/(24*3600)*pi) y[tSeq$wday==0
## | tSeq$wday==6] <- 0 Harmonics[ ,paste0('cosWork',i)] <- y }

## ## For holy days for(i in 1:n_harmonics) { ## Sine term y <-
## sin(i*2*x/(24*3600)*pi) ## Set to NA if working day. wday has sunday==0
## y[tSeq$wday %in% 1:5] <- 0 Harmonics[ ,paste0('sinHoly',i)] <- y ## Cos term y <-
## cos(i*2*x/(24*3600)*pi) y[tSeq$wday %in% 1:5] <- 0 Harmonics[
## ,paste0('cosHoly',i)] <- y }

