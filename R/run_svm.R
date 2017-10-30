run_svm <- function (indat, kern = 1) 
{
    kernels <- c("linear", "polynomial", "radial", "sigmoid")
    dat <- indat$data
    dat$class <- as.factor(dat$class)
    mlmod <- svm(class ~ ., dat, kernel = kernels[kern])
    pred <- predict(mlmod, dat, type = "class")
    pred <- cbind(dat[, 1:2], class = pred)
    return(list(data = pred, xylim = indat$xylim))
}
