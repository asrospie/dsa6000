same_birthday <- function(n) {
    return(1 - (prod((365-n+1):365) / 365^n))
}

