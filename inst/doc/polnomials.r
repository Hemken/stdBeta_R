fitsq <- lm(mpg ~ wt + I(wt^2), mtcars)
summary(fitsq)
# head(model.frame(fitsq))
modelvars <- all.vars(formula(fitsq))
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fitsq, data=mtcars))

fit3 <- lm(mpg ~ (wt + I(wt^2))*disp, datasets::mtcars)
summary(fit3)

modelvars <- all.vars(formula(fit3))
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fit3, data=mtcars))

rm(mtcars)
stdBeta(fit3)

head(poly(mtcars$wt, degree=2, raw=TRUE))

fit4 <- lm(mpg ~ poly(wt, degree=2, raw=TRUE)*disp, mtcars)
summary(fit4)
stdBeta(fit4)

modelvars <- all.vars(formula(fit4))
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fit4, data=mtcars))
