per <- function(tstart, time, tend) {
    if (class(tstart)[1] == "character") 
        tstart <- asP(tstart)
    if (class(tend)[1] == "character") 
        tend <- asP(tend)
    asP(tstart) < time & time <= asP(tend)
}


TRUE
