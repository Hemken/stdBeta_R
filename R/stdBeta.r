stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- stats::model.frame(lmfit)
  # print(str(stddata))
  # print(sapply(stddata, data.class))
  stopifnot(all(sapply(stddata, data.class) %in% c("numeric", "factor", "AsIs")))
  modelvars <- all.vars(formula(lmfit))
  # print(modelvars)
  numvars <- sapply(stddata[, modelvars], is.numeric)
  stddata[, modelvars][, numvars] <- sapply(stddata[, modelvars][, numvars], scale)
  # head(df)
  stats::update(lmfit, data=stddata)
}
