fit <- lm(mpg ~ disp*wt, mtcars)
mtcars$Cyl <- as.factor(mtcars$cyl)
fit2 <- lm(mpg ~ disp*Cyl, mtcars)

stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- model.frame(lmfit)
  # print(str(df))
  numvars <- sapply(stddata, is.numeric)
  stddata[, numvars] <- sapply(stddata[numvars], scale)
  # head(df)
  update(lmfit, data=stddata)
}

fit
stdBeta(fit)
fit2
stdBeta(fit2)
