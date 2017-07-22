stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- stats::model.frame(lmfit)
  modelvars <- all.vars(formula(lmfit))

  numvars <- sapply(stddata[, modelvars], is.numeric)
  stddata[modelvars][, numvars] <- sapply(stddata[modelvars][numvars], scale)
  # head(df)
  stats::update(lmfit, data=stddata)
}
