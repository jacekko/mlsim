run_rpart <- function (indat, dp = 30) 
{
    dat <- indat$data
    mlmod <- rpart(class ~ ., dat, control = rpart.control(maxdepth = dp))
    pred <- predict(mlmod, dat, type = "class")
    pred <- cbind(dat[, 1:2], class = pred)
    return(list(data = pred, xylim = indat$xylim))
}
