run_rfsrc <- function (indat, nt = 50) 
{
    dat <- indat$data
    dat$class <- as.factor(dat$class)
    mlmod <- rfsrc(class ~ ., data = dat, ntree = nt)
    pred <- predict(mlmod, dat)
    pred <- cbind(dat[, 1:2], class = as.character(pred$class))
    return(list(data = pred, xylim = indat$xylim))
}
