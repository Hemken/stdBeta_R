# Pure polynomial, by hand
fitsq <- lm(mpg ~ wt + I(wt^2), mtcars)
# summary(fitsq)
# head(model.frame(fitsq))
modelvars <- all.vars(formula(fitsq))
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fitsq, data=mtcars))

QuantPsyc::lm.beta(fitsq) # Wrong, no warning!
lm.beta::lm.beta(fitsq) # same problem
lsr::standardCoefs(fitsq) # and again
arm::standardize(fitsq, standardize.y=TRUE) # wrong, 2x
stdBeta::stdBeta(fitsq) # CORRECT

# Polynomial plus interaction
fit3 <- lm(mpg ~ (wt + I(wt^2))*disp, datasets::mtcars)
modelvars <- all.vars(formula(fit3))
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fit3, data=mtcars))
rm(mtcars)

stdBeta::stdBeta(fit3) # CORRECT
arm::standardize(fit3, standardize.y=TRUE) # ERROR
QuantPsyc::lm.beta(fit3) # Wrong, with WARNING
lm.beta::lm.beta(fit3) # wrong, no warning
lsr::standardCoefs(fit3) # again wrong, no warning

# Polynomial using poly()
# head(poly(mtcars$wt, degree=2, raw=TRUE))
fit4 <- lm(mpg ~ poly(wt, degree=2, raw=TRUE)*disp, mtcars)
modelvars <- all.vars(formula(fit4))
# note here we go back to the data, not the lm object
mtcars[, modelvars] <- sapply(mtcars[, modelvars], scale)
summary(update(fit4, data=mtcars))
rm(mtcars)

stdBeta::stdBeta(fit4) # blocked
QuantPsyc::lm.beta(fit4) # Wrong, with WARNING
lm.beta::lm.beta(fit4) # wrong, no warning
lsr::standardCoefs(fit4) # again wrong, no warning
arm::standardize(fit4, standardize.y=TRUE) # wrong, but no error
