run_logreg <- function (indat, thr = 0.5) 
{
    dat <- indat$data
    dat$class <- ifelse(dat$class == "C", 1, 0)
    mlmod <- glm(class ~ ., data = dat, family = binomial("logit"))
    lreg <- predict.glm(mlmod, dat, type = "response")
    dat$class <- ifelse(lreg >= thr, "C", "T")
    return(list(data = dat, xylim = indat$xylim))
}
