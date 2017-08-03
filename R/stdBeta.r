stdBeta <- function(lmfit) {
  stopifnot(class(lmfit)=="lm")
  stddata <- stats::model.frame(lmfit, data=lmfit$call$data)
  # print(str(stddata))
  framenames <- names(stddata)
  # print(framenames)
  modelvars <- all.vars(formula(lmfit))
  # print(modelvars)
  if (length(framenames)==length(modelvars)) {
    if (length(modelvars[framenames != modelvars])>0) { # indicates a matrix as var
      # print(lmfit$call$data)
      stddata <- get_all_vars(formula(lmfit), data=eval(lmfit$call$data))
    }
  }
  # print(sapply(stddata, data.class))
  stopifnot(all(sapply(stddata, data.class) %in% c("numeric", "factor", "AsIs")))
  numvars <- sapply(stddata[, modelvars], is.numeric)
  stddata <- stats::na.omit(stddata[, modelvars])
  stddata[, numvars] <- sapply(stddata[, numvars], scale)
  # head(df)
  stats::update(lmfit, data=stddata)
}
