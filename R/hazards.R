
cumulative_hazard <- function(object, xnew, event = 1, times = NULL, use_initial_fit = FALSE) {
  
  if (use_initial_fit) {
    # cv.glmnet object with lambda.min
    model_fit <- object$initial_fits[[event]]
    lambda_min <- model_fit$lambda.min
  } else {
    # glmnet fit object within fwelnet object within cv.fwelnet object
    cvfit <- object$fwelfits[[event]]
    model_fit <- cvfit$glmfit$glmfit
    # survfit() looks up `s` on the internal glmnet's lambda grid, which fwelnet() scales
    # by mean(pen.weights); map the nominal lambda.min onto that grid so the hazard uses
    # the same coefficients as predict()'s linear predictor (else they diverge as theta grows).
    lambda_min <- model_fit$lambda[which(cvfit$lambda == cvfit$lambda.min)]
  }
  # browser()
  bfit <- survival::survfit(
    model_fit, 
    x = object$x,
    y = object$y[[event]],
    newx = x_modelmat(object, xnew), s = lambda_min
  )

  # survfit() drops cumhaz to a vector for a single observation; keep it times x obs
  ret <- bfit$cumhaz
  if (is.null(dim(ret))) ret <- matrix(ret, ncol = 1)

  if (!is.null(times)) {
    # Make sure to subset by [rows, columns, drop = FALSE]
    # Don't forget the second , or you will have a bad time
    # Ask me how I know
    ret <- ret[which(bfit$time %in% times), , drop = FALSE]
  }
  
  ret
}
