library(riskRegression)
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

test_that("low dim fitting", {
  set.seed(1)
  train <- getpbc(500)
  test <- getpbc(300)
  
  fit <- suppressWarnings(cooper(train, standardize = TRUE, nfolds = 3, stratify_by_status = TRUE, mt_max_iter = 3))
  coef_names <- c("sex", "age", "trt", "logbili", "logprotime", "protimegrp10-11", "protimegrp>11", "stage3", "stage4")
  
  for (i in 1:2) {
    testcoefs <- coef(fit, event = i, s = "lambda.min")
    checkmate::expect_atomic_vector(testcoefs, any.missing = FALSE, len = 9)
    expect_named(testcoefs, expected = coef_names)
  }
  
  # Assumed default is correct
  expect_identical(
    coef(fit, event = 1, s = "lambda.min"),
    coef(fit)
  )
  
  # Internally stored glmnet obj has same coefs as final cooper fwelnet
  for (i in 1:2) {
    expect_equal(
      coef(fit, event = i, s = "lambda.min"),
      fit$fwelfits[[i]]$glmfit$glmfit$beta[, fit$fwelfits[[i]]$glmfit$lambda == fit$fwelfits[[i]]$lambda.min]
    )
  }
  

})

test_that("low dim predict: risk", {
  set.seed(1)
  train <- getpbc(500)
  test <- getpbc(300)
  
  fit <- suppressWarnings(cooper(train, standardize = TRUE, nfolds = 3, stratify_by_status = TRUE))
  
  eval_times <- quantile(train$time, seq(0.1, 0.5, 0.1), type = 2, names = FALSE)
  
  res <- predict(fit, xnew = test, event = 1, type = "risk")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("id", "event", "lp", "eXb"))
  
  expect_identical(
    predict(fit, xnew = test, event = 1, type = "risk"),
    predict(fit, xnew = test, type = "risk")
  )
  
  expect_failure(
    expect_identical(
      predict(fit, xnew = test, event = 2, type = "risk"),
      predict(fit, xnew = test, type = "risk")
    )
  )
  
})

test_that("low dim predict: absrisk", {
  set.seed(1)
  train <- getpbc(500)
  test <- getpbc(300)
  
  fit <- suppressWarnings(cooper(train, standardize = TRUE, nfolds = 3, stratify_by_status = TRUE))
  
  eval_times <- quantile(train$time, seq(0.1, 0.5, 0.1), type = 2, names = FALSE)
  
  res <- predict(fit, xnew = test, times = eval_times, type = "absrisk")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("id", "event", "horizon", "absolute_risk"))
  expect_equal(nrow(res), nrow(test) * length(eval_times))
  
  expect_identical(
    predict(fit, xnew = test, times = eval_times, event = 1, type = "absrisk"),
    predict(fit, xnew = test, times = eval_times, type = "absrisk")
  )
  
  expect_error(predict(fit, xnew = test, type = "absrisk"))
})


test_that("various sanity checks", {
  set.seed(1)
  train <- getpbc(500)
  expect_error(cooper(train, standardize = TRUE, nfolds = 2))
  expect_warning(expect_error(cooper(train, standardize = TRUE, nfolds = 4, mt_max_iter = 1)))
})


test_that("low dim fitting with different variable names", {
  set.seed(1)
  train <- getpbc(300)

  set.seed(3)
  fit0 <- cooper(
    train,
    standardize = TRUE, 
    nfolds = 3, 
    stratify_by_status = TRUE, 
    mt_max_iter = 3
  )
  
  train$othertime <- train$time
  train$time <- NULL
  
  train$event <- train$status
  train$status <- NULL
  
  set.seed(3)
  fit <- cooper(
    train, time = "othertime", status = "event",
    standardize = TRUE, 
    nfolds = 3, 
    stratify_by_status = TRUE, 
    mt_max_iter = 3
  )
  
  # Default should be equivalent, no change in estimated coefs or anything
  expect_equal(fit0, fit)
  
  
  
})
