\name{stdBeta}
\alias{stdBeta}
\title{Standardized Coefficients for Linear Models}
\usage{
stdBeta(lmfit)
}
\description{
Updates a model fit by substituting standarized variables.
}
\arguments{
\item{lmfit}{A fit object of class "lm".}
}
\value{
A fit object, of class "lm"
}
\author{
Doug Hemken, SSCC, Univ. of Wisconsin-Madison
}
\examples{
fit <- lm(mpg ~ disp*wt, mtcars)
stdBeta(fit)

# Does not standardize factors, only numeric variables
mtcars$Cyl <- as.factor(mtcars$cyl)
fit2 <- lm(mpg ~ disp*Cyl, mtcars)
stdBeta(fit2)

}
