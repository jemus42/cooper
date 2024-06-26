% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.fwelnet.R
\name{predict.fwelnet}
\alias{predict.fwelnet}
\title{Make predictions from a "fwelnet" object}
\usage{
\method{predict}{fwelnet}(object, xnew, type = c("link", "response", "coefficients"), ...)
}
\arguments{
\item{object}{Fitted "\code{fwelnet}" object.}

\item{xnew}{Matrix of new values for \code{x} at which predictions are to
be made.}

\item{type}{Type of prediction required. Type \code{"link"} (default) gives the
linear predictors. Type \code{"response"} gives the linear predictor for "gaussian"
and fitted probabilities for "binomial" family.
\code{"coefficients"} will return the beta vector or matrix.}

\item{...}{Potentially other arguments to be passed to and from methods;
currently not in use.}
}
\value{
Predictions which the model \code{object} makes at \code{xnew}.
}
\description{
This function returns the predictions from a "\code{fwelnet}" object
for a new data matrix.
}
\examples{
set.seed(1)
n <- 100; p <- 20
x <- matrix(rnorm(n * p), n, p)
beta <- matrix(c(rep(2, 5), rep(0, 15)), ncol = 1)
y <- x \%*\% beta + rnorm(n)
z <- cbind(1, abs(beta) + rnorm(p))

fit <- fwelnet(x, y, z)
predict(fit, xnew = x[1:5, ])

biny <- ifelse(y > 0, 1, 0)
fit <- fwelnet(x, biny, z, family = "binomial")
# linear predictor values
predict(fit, xnew = x[1:5, ])
# fitted probabilities
predict(fit, xnew = x[1:5, ], type = "response")

}
\seealso{
\code{\link[=fwelnet]{fwelnet()}}.
}
