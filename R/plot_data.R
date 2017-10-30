plot_data <- function (dat, col1 = "orange", col2 = "grey48") 
{
    tmp <- dat$data
    lwr <- dat$xylim[1]
    upr <- dat$xylim[2]
    plot(NULL, xlim = c(lwr, upr), ylim = c(lwr, upr), xlab = "", 
        ylab = "")
    points(tmp[tmp$class == "C", 1:2], , col = col1, pch = 1)
    points(tmp[tmp$class == "T", 1:2], col = col2, pch = 2, cex = 0.7)
}
