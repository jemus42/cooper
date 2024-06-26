% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.cooper.R
\name{predict.cooper}
\alias{predict.cooper}
\title{Survival prediction for COoper models.}
\usage{
\method{predict}{cooper}(
  object,
  xnew,
  type = c("absrisk", "risk"),
  event = 1,
  times = NULL,
  use_initial_fit = FALSE,
  reference = c("zero", "sample"),
  ...
)
}
\arguments{
\item{object}{Object of class \code{cooper}, containing two cause-specific Cox model fits.}

\item{xnew}{\code{data.frame} with new observations for predictions.}

\item{type}{Either \code{"risk"} for linear predictors, or \code{absrisk} (default) for CIF.}

\item{event}{Event of interest, either \code{1} (default) or \code{2}.}

\item{times}{Event times to evaluate at.}

\item{use_initial_fit}{(\code{FALSE}) If \code{TRUE}, uses the initial \code{glmnet} fit of the cooper algorithm.}

\item{reference}{\strong{NYI} Either \code{"zero"} or \code{"sample"}.}

\item{...}{Unused.}
}
\value{
For \code{type = "risk"}, a data.frame with columns "id", "event", "lp", "eXb".
For \code{type = "absrisk"}, a data.frame with columns "id" "event" "horizon" "absolute_risk".
}
\description{
Survival prediction for COoper models.
}
\examples{

data(pbc, package = "survival")
pbc <- na.omit(pbc)
xtrain <- pbc[1:270, -1]
xtest <- pbc[270:276, -1]

set.seed(12)
cooperfit <- cooper(
  xtrain, mt_max_iter = 3,
  stratify_by_status = TRUE,
  standardize = FALSE # Currently required to ensure correct results
)

predict(cooperfit, xnew = xtest, type = "risk", event = 1)
predict(cooperfit, xnew = xtest, times = c(1180.5, 1788), type = "absrisk", event = 1)
}
