run_xgboost <- function (indat, nr = 20, md = 10, thr = 0.5) 
{
    dat <- indat$data
    dat$class <- ifelse(dat$class == "C", 1, 0)
    mdat <- model.matrix(object = ~0 + ., data = dat)
    Y <- mdat[, 3]
    X <- mdat[, 1:2]
    mdat <- xgb.DMatrix(data = X, label = Y)
    mlmod <- xgb.train(data = mdat, objective = "binary:logistic", 
        booster = "gbtree", nrounds = nr, nthread = 1, params = list(maxdepth = md))
    pred <- predict(mlmod, mdat)
    dat$class <- ifelse(pred >= thr, "C", "T")
    return(list(data = dat, xylim = indat$xylim))
}
