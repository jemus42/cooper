% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util-cv.R
\name{stratified_cv_folds}
\alias{stratified_cv_folds}
\title{Stratified CV folds for \code{cv.fwelnet}}
\usage{
stratified_cv_folds(xdf, nfolds = 10L)
}
\arguments{
\item{xdf}{An input dataset with at least \code{"status"} variable.}

\item{nfolds}{(\code{10}) Integer number of CV folds, default is equal to that of \code{\link{cv.glmnet}}.}
}
\value{
A \code{data.frame} with variables \code{folds} for fold assignments and \code{status} for checking proportions.
}
\description{
Stratified CV folds for \code{cv.fwelnet}
}
\examples{
xdf <- data.frame(
  row_id = 1:50, 
  status = c(rep(0, 20), rep(1, 18), rep(2, 12)) 
)

resdf <- stratified_cv_folds(xdf, nfolds = 10)
table(resdf$status, resdf$fold)
}
