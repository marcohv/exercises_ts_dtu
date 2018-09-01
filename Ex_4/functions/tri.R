tri <- function(x_i, x, h){ 
    val <- 1 - abs((x - x_i) / h)
    val[val < 0] <- 0
    return(val)
}
