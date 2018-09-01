cache_save <- function(fit, cache_dir, cache_name){
    dir.create(cache_dir, showWarnings=FALSE, recursive=TRUE)
    saveRDS(fit, paste0(cache_dir,"//", cache_name))
}
