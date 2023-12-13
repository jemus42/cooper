#' Wrapper around riskRegression::simPBC
#'
#' @noRd
#' @keywords internal
getpbc <- function(n = 100) {
  if (!requireNamespace("riskRegression", quietly = TRUE)) {
    stop("riskRegression is needed for this functionality")
  }
  xp <- c("time", "status", "sex", "age", "trt", "logbili", "logprotime", "protimegrp", "stage")
  riskRegression::simPBC(n = n)[xp]
}
