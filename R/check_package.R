check_package <- function (pkg, opt) 
{
    erf <- function(x) {
    }
    clf <- function(x) {
        return(NULL)
    }
    cint <- tryCatch(eval(parse(text = paste("library(", pkg, 
        ")", sep = ""))), error = erf, finally = clf)
    if (is.null(cint)) {
        cat(paste("You need to install package", pkg, "to", opt, "\n"))
        return(F)
    }
    else return(T)
}
