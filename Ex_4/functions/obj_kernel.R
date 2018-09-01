## Wrap the leave-one-out in a function which returns the score
obj_kernel <- function(h, frml, data, k = k, ieval = 1:nrow(data), n_min=10, local_second_order = TRUE, return_yhat = FALSE){
    ## Should second order terms be added to the data and frml
    if(local_second_order){
        ## Input variables
        nms <- all.vars(as.formula(frml))[-1]
        tmp <- sapply(nms, function(nm){
            data[ ,nm]^2
        })
        colnames(tmp) <- pst(nms,"2")
        data <- cbind(data,tmp)
        frml <- paste(frml,"+",pst(nms,"2",collapse=" + "))
    }
    ## Keep the output in yhat, only for ieval points
    yhat <- sapply(ieval, function(i){
        ## check if there are data to fit
        if((i-k) < n_min){ return(NA) }
        if(any(is.na(data[i,names(h)]))){ return(NA) }
        ## Only use values available k steps behind (otherwise future values would be used)      
        ipast <- 1:(i-k)
        ## Calculate the weights for the variables in h
        tmp <- sapply(names(h), function(nm){
            epanechnikov(data[i,nm], data[ipast,nm], h=h[nm])
        })
        w <- apply(tmp,1,prod)
        if(sum(!is.na(w)) < n_min){ return(NA) }
        ## Only everything before the i'th observation will be included in the fit
        fit <- lm(as.formula(frml), data[ipast, ], weights = w)
        ## Now predict for the i point (for the k'th horizon)
        predict(fit, newdata = data[i, ])
    })
    ##
    if(return_yhat){
        return(yhat)
    }else{
        ## The score value
        nm <- all.vars(as.formula(frml))[1]
        val <- rmse(data[ieval,nm] - yhat)
        ##
        print(pst("h = c(",pst(names(h)," = ",h,collapse=", "),"), val = ",val))
        ##
        return(val)
    }
}
