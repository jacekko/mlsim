datagen <- function (lwr, upr, fun, N, contam = 0) 
{
    dat <- NULL
    for (i in 1:N) {
        x <- runif(n = 2, min = lwr, max = upr)
        if (fun(x[1], x[2]) > 0) 
            dat <- rbind(dat, data.frame(x = x[1], y = x[2], class = "C"))
        else dat <- rbind(dat, data.frame(x = x[1], y = x[2], class = "T"))
    }
    if (contam > 0) {
        N <- floor(contam * N)
        for (i in 1:N) {
            x <- runif(n = 2, min = lwr, max = upr)
            if (fun(x[1], x[2]) < 0) 
                dat <- rbind(dat, data.frame(x = x[1], y = x[2], class = "C"))
            else dat <- rbind(dat, data.frame(x = x[1], y = x[2], class = "T"))
        }
    }
    return(list(data = dat, xylim = c(lwr, upr)))
}
