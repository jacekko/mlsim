run_nnet <- function (indat, sz = 15) 
{
    dat <- indat$data
    dat$class <- as.factor(dat$class)
    mlmod <- nnet(class ~ ., dat, size = sz, contrasts = levels(dat$class), 
        trace = F)
    pred <- predict(mlmod, dat, type = "class")
    pred <- cbind(dat[, 1:2], class = pred)
    return(list(data = pred, xylim = indat$xylim))
}
