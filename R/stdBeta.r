stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- model.frame(lmfit)
  # print(str(df))
  numvars <- sapply(stddata, is.numeric)
  stddata[, numvars] <- sapply(stddata[numvars], scale)
  # head(df)
  update(lmfit, data=stddata)
}
