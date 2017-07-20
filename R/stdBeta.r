stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- stats::model.frame(lmfit)
  # print(str(df))
  numvars <- sapply(stddata, is.numeric)
  stddata[, numvars] <- sapply(stddata[numvars], scale)
  # head(df)
  stats::update(lmfit, data=stddata)
}
