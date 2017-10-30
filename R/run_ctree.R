run_ctree <- function (indat, dp = 0) 
{
    dat <- indat$data
    dat$class <- as.factor(dat$class)
    mlmod <- ctree(class ~ ., dat, controls = ctree_control(maxdepth = dp))
    pred <- predict(mlmod, dat, type = "response")
    pred <- cbind(dat[, 1:2], class = as.character(pred))
    return(list(data = pred, xylim = indat$xylim))
}
