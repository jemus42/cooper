<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cooperative Penalized Regression (CooPeR)

⚠️ Fork Notice ⚠️

This repository is a fork of the original [`fwelnet` implementation by
Tay et al](https://github.com/kjytay/fwelnet).  
The goal of this fork is support a research project based on the
extension of `fwelnet` to survival settings and competing event
analysis. For this purpose, some of the internal functions were modified
in a way that is not directly compatible with the original
implementation, hence this project currently stands on its own, rather
than being a pull-request to the original implementation.

This version, renamed to `cooper`, supports targets of the form
`Surv(time, event)` from the `survival` package, and fits `fwelnet`
through the minimization of the Cox deviance. In addition, it adds the
`cooper()` function for a specific implementation of the multi-task
algorithm proposed by Tay et al. which fits cause-specific Cox models.
As of now, the implementation is tailored towards a specific research
question rather than general use.

Content from the original repository follows.

# Feature-Weighted Elastic Net (fwelnet)

`fwelnet` is a package that fits the ***feature-weighted elastic net
(fwelnet)***, a variant of the elastic net which has feature-specific
penalties. These penalties are based on additional information that the
user has on the features. `fwelnet` works with continuous and binary
responses. For details, please see the
[preprint](https://arxiv.org/abs/2006.01395). For a short tutorial on
how to use the package, please see the vignette in the `vignettes/`
folder.

## An example

Here is a simple example to illustrate how to use this package. First,
let’s generate some data. In this example, we assume that we have 40
features, and that these features come in 4 groups of 10. The response
is a linear combination of the features from the first 2 groups with
additive Gaussian noise.

``` r
set.seed(1)
n <- 100
p <- 40
groups <- list(1:10, 11:20, 21:30, 31:40)  # which features belong to which group
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- matrix(rep(1:0, each = 20), ncol = 1)
y <- x %*% beta + rnorm(n)
```

In order to fit a fwelnet model, we have to define a feature information
matrix. In our example, we have
$\mathbf{Z} \in \mathbb{R}^{40 \times 4}$, with
$z_{jk} = 1\{ \text{feature } j \text{ belongs to group } k \}$.

``` r
# generate Z matrix
z <- matrix(0, nrow = p, ncol = length(groups))
for (i in 1:length(groups)) {
    z[groups[[i]], i] <- 1
}
```

Once `z` is specified, we can fit the fwelnet model with `fwelnet()`.

``` r
library(fwelnet)
fit <- fwelnet(x, y, z)
```

“fwelnet” objects are equipped with `predict` and `coef` methods which
allow the user to make predictions on new data and to view the model
coefficients. By default predictions and coefficients are returned for
the whole `lambda` path.

``` r
# predictions for first 5 observations at 20th lambda value
predict(fit, x[1:5, ])[, 20]
#  [1]  1.2359876  6.3969524 -1.9602492 -1.1781763 -0.5872078

# coefficients at the 20th lambda value (including intercept)
as.numeric(coef(fit)[, 20])
#   [1] -0.1440444  0.6481368  0.5835123  0.5545719  0.7626670  0.9323669
#   [7]  0.7142652  0.7729034  1.1863878  0.9510465  0.7041017  0.7242916
#  [13]  0.7258272  0.9641891  0.7273089  0.7733351  0.8912650  1.2407252
#  [19]  0.5566069  1.1192067  0.7399865  0.0000000  0.0000000  0.0000000
#  [25]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#  [31]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#  [37]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```
