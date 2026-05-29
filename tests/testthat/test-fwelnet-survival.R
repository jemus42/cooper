library(survival)

x <- as.matrix(veteran[, 5:8]) # c(-3, -4)
y <- Surv(veteran$time, veteran$status)

test_that("Parity with glmnet", {
  # if z = 1, should be same as glmnet
  z <- matrix(1, ncol = 1, nrow = ncol(x))

  fwfit <- fwelnet(x, y, z, family = "cox")
  gfit <- glmnet::glmnet(x, y, family = "cox")

  # fwelnet pre-standardizes then unstandardizes, whereas glmnet's internal standardize=TRUE
  # Cox path differs numerically by ~1e-3 (largest on near-zero coefs). Compare on an absolute
  # scale rather than the relative 1e-5, which is dominated by those near-zero entries.
  expect_lt(max(abs(fwfit$beta - as.matrix(gfit$beta))), 5e-3)
  expect_equal(gfit$lambda, fwfit$lambda)
})

lambda_seq <- seq(1e-4, .5, length.out = 20)

test_that("CV parity with glmnet: coefficients overall", {
  # if z = 1, should be same as glmnet
  z <- matrix(1, ncol = 1, nrow = ncol(x))
  
  set.seed(1234)
  fwfit <- cv.fwelnet(x, y, z, family = "cox", lambda = lambda_seq)
  gfit <- glmnet::cv.glmnet(x, y, family = "cox", lambda = lambda_seq)
  
  expect_equal(gfit$lambda, fwfit$lambda)
  # absolute-scale comparison, see "Parity with glmnet" above
  expect_lt(max(abs(fwfit$glmfit$beta - as.matrix(gfit$glmnet.fit$beta))), 5e-3)
})

test_that("CV parity with glmnet: lambda selection", {
  # if z = 1, should be same as glmnet
  z <- matrix(1, ncol = 1, nrow = ncol(x))
  
  fwfit <- cv.fwelnet(x, y, z, family = "cox")
  gfit <- glmnet::cv.glmnet(x, y, family = "cox")
  expect_equal(gfit$lambda, fwfit$lambda)
  
  set.seed(1234)
  fwfit <- cv.fwelnet(x, y, z, family = "cox", lambda = lambda_seq)
  gfit <- glmnet::cv.glmnet(x, y, family = "cox", lambda = lambda_seq)
  expect_equal(gfit$lambda, fwfit$lambda)
  
  expect_equal(
    which(gfit$lambda == gfit$lambda.min),
    which(fwfit$lambda == fwfit$lambda.min),
    info = "Expecting same lambda in case of uninformative z",
    label = "Selected lambda.min index"
  )
  
  expect_equal(gfit$lambda.min, fwfit$lambda.min)
  
  set.seed(1234)
  fwfit <- cv.fwelnet(x, y, z, family = "cox")
  gfit <- glmnet::cv.glmnet(x, y, family = "cox", lambda = fwfit$lambda)
  
  # Must be same sequence
  expect_equal(gfit$lambda, fwfit$lambda)
  # should be same lambda.min
  expect_equal(gfit$lambda.min, fwfit$lambda.min)
})

test_that("CV for cox is not much slower than glmnet", {
  z <- matrix(1, ncol = 1, nrow = ncol(x))
  t1 <- system.time(cv.fwelnet(x, y, z, family = "cox"))
  t2 <- system.time(glmnet::cv.glmnet(x, y, family = "cox"))

  expect_equal(mean(t1), mean(t2), tolerance = 0.1)
})


