cache_load <- function(rerun, X, cache_dir){
    ## Get the name of the function from which was called
    fun_nm <- strsplit(deparse(sys.calls()[[sys.nframe()-1]]), "\\(")[[1]][1]
    ## Create the md5 checksum filename with digest
    require("digest")
    file_name <- paste0(fun_nm,"_",digest(c(X,format(get(fun_nm)))),".RDS")

    if(rerun){
        if(file_name %in% dir(cache_dir)){
            fit <- readRDS(gsub("//","/",paste0(cache_dir,"/",file_name))) 
            return(fit)
        }
    }
    return(file_name)
}
