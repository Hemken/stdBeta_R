library(stdBeta)

fit <- lm(mpg ~ disp*wt, mtcars)
mtcars$Cyl <- as.factor(mtcars$cyl)
fit2 <- lm(mpg ~ disp*Cyl, mtcars)

fit
stdBeta(fit)
fit2
stdBeta(fit2)
