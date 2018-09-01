setpar <- function(tmpl=NA,...)
  {
    ## Get par list
    ##p <- par(no.readonly=TRUE)
    ## Templates
    if(!is.na(tmpl))
      {
        if(tmpl=="ts")
          {
            par(mfrow=c(3,1),
                oma = c(3,0,2,2),
                mar = c(0,3.5,0.5,0),
                mgp = c(2.2,0.6,0),
                tcl = -0.4,
                xaxt = "n"
                )
          }
      }
    ## Replace all the parameters given in prm
    ## Get only the ... parameters
    i <- which(!names(match.call()) %in% names(match.call(expand.dots=FALSE)))
    if(length(i)>0)
      {
        par(...)
        ## prm <- as.list(match.call()[i])
        ## p <- list()
        ## for(i in 1:length(prm))
        ##   {
        ##     p$new <- eval(prm[[i]])
        ##     names(p)[i] <- names(prm)[i]
        ##   }
        ## par(p)
      }
    ## Set par and return the original par
    #options(warn = (-1))
    #options(warn = 1)
  }
