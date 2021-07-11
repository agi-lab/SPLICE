#############################################################################
##                     Statistical Helper Functions                        ##
#############################################################################

#' Inverse Tranform Sampling
#'
#' Generates sample numbers at random from any probability distribution
#' given its cumulative distribution function. Pre-defined distribution
#' functions such as \code{pnorm} are supported. \cr \cr
#' See \href{https://en.wikipedia.org/wiki/Inverse_transform_sampling}{here}
#' for the algorithm.
#'
#' @param n number of observations.
#' @param cdf cumulative distribution function to be sampled from.
#' @param range support of the given \code{cdf}.
#' @param ... other arguments/parameters to be passed onto \code{cdf}.
#' @examples
#' simulate_cdf(10, pnorm)
#' simulate_cdf(10, pbeta, shape1 = 2, shape2 = 2)
#' @export
simulate_cdf <- function(n, cdf, range = c(-1e200, 1e200), ...) {

  solve_cdf <- function(x, u) {
    cdf(x, ...) - u
  }

  # quantile_cdf returns the quantile for a distribution defined by cdf
  quantile_cdf <- function(u) {
    stats::uniroot(solve_cdf, interval = range, tol = 1e-16, u = u)$root
  }
  quantile_cdf <- Vectorize(quantile_cdf)

  prob <- stats::runif(n)
  quantile_cdf(prob)

}


#' Estimating Weibull Parameters
#'
#' Returns the Weibull shape and scale parameters given the mean and the CoV
#' of the target Weibull distribution.
#'
#' @param target_mean mean of the target Weibull distribution.
#' @param target_cv CoV of the target Weibull distribution.
#' @examples
#' get_Weibull_parameters(target_mean = 100000, target_cv = 0.60)
#' get_Weibull_parameters(target_mean = c(100000, 200000, 300000),
#'                        target_cv = 0.60)
#' @export
get_Weibull_parameters <- function(target_mean, target_cv) {

  # f: a helper function used to determine the Weibull shape and scale parameters from mean and CV
  # by computing the difference between the CV implied by a guess shape parameter and the target CV
  f <- function(a, cv) {
    (sqrt(gamma(1 + 2/a) - (gamma(1 + 1/a))^2)/gamma(1 + 1/a)) - cv
  }

  Weibull_shape <- stats::uniroot(f, cv = target_cv, lower = 0.1, upper = 100, tol = 1e-16)$root
  Weibull_scale <- target_mean / gamma(1 + 1/Weibull_shape)

  c(Weibull_shape, Weibull_scale)
}

get_Weibull_parameters <- Vectorize(get_Weibull_parameters)


#' Estimating Beta Parameters
#'
#' Returns the Beta parameters given the mean and the CoV of the target Beta
#' distribution.
#'
#' @param target_mean mean of the target Beta distribution (between 0 and 1).
#' @param target_cv CoV of the target Beta distribution.
#' @examples
#' get_Beta_parameters(target_mean = 0.5, target_cv = 0.20)
#' get_Beta_parameters(target_mean = 0.5,
#'                     target_cv = c(0.10, 0.20, 0.30))
#' @export
get_Beta_parameters <- function(target_mean, target_cv) {
  target_var <- (target_cv * target_mean)^2
  alpha <- ((1 - target_mean)/target_var - 1/target_mean) * target_mean^2
  beta <- alpha * (1/target_mean - 1)
  return(c(alpha, beta))
}
get_Beta_parameters <- Vectorize(get_Beta_parameters)


#' Coefficient of Variation
#'
#' Returns the observed coefficient of variation (CoV) of a given sample
#' \code{x}. \cr \cr
#' If \code{na.rm} is true then missing values are removed before
#' computation proceeds, as in the case of the \code{mean()} function.
#'
#' @param x a numeric vector.
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @details The coefficient of variation is defined as is defined as the
#' ratio of the standard deviation to the mean. It shows the extent of
#' variability in relation to the mean of the population. \cr \cr
#' \code{cv()} estimates the CoV of a given sample by computing the ratio of
#' the sample standard deviation (see \code{stats::sd}) to the sample mean.
#' @examples
#' cv(1:10)
#' @export
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}


#' The Triangular Distribution
#'
#' Density, distribution function, quantile function and random generation for
#' the triangular distribution with parameters `min`, `max` and `mode`.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If `length(n) > 1`, the length is taken to
#' be the number required.
#' @param min vector of minimum values.
#' @param max vector of maximum values.
#' @param mode vector of modes.
#' @details The triangular distribution with parameters `min`\eqn{= a}, `max`
#' \eqn{= b}, `mode`\eqn{= c} has density:
#' \tabular{lll}{
#' \eqn{f(x) =}
#' \tab \eqn{\frac{2(x-a)}{(b-a)(c-a)}} \tab for \eqn{a < x \le c} \cr
#' \tab \eqn{\frac{2(b-x)}{(b-a)(b-c)}} \tab for \eqn{c < x \le b} \cr
#' \tab \eqn{0} \tab otherwise
#' } and distribution function:
#' \tabular{lll}{
#' \eqn{F(x) =}
#' \tab \eqn{0} \tab for \eqn{x \le a} \cr
#' \tab \eqn{\frac{(x-a)^2}{(b-a)(c-a)}} \tab for \eqn{a < x \le c} \cr
#' \tab \eqn{1 - \frac{(b-x)^2}{(b-a)(b-c)}} \tab for \eqn{c < x \le b} \cr
#' \tab \eqn{1} \tab for \eqn{x > b}
#' }
#' for \eqn{a \le c \le b}.
#' @return `dtri` gives the density, `ptri` gives the distribution function,
#' `qtri` gives the quantile function, and `rtri` generates random deviates.
#' @name Triangular
NULL

#' @rdname Triangular
#' @examples
#' ptri(c(0, 1/2, 1), min = 0, max = 1, mode = 1/2)
#' @export
ptri <- function(q, min, max, mode) {
  if (q <= min) {
    0
  } else if (min < q & q <= mode) {
    (q - min)^2 / ((mode - min) * (max - min))
  } else if (mode < q & q <= max) {
    1 - (max - q)^2 / ((max - min) *  (max - mode))
  } else {
    1
  }
}
ptri <- Vectorize(ptri)

#' @rdname Triangular
#' @examples
#' dtri(c(0, 1/2, 1), min = 0, max = 1, mode = 1/2)
#' plot(function(x) dtri(x, min = 0, max = 1, mode = 1/2), 0, 1)
#' @export
dtri <- function(x, min, max, mode) {
  if (min < x & x <= mode) {
    2 * (x - min) / ((max - min) * (mode - min))
  } else if (mode < x & x <= max) {
    2 * (max - x) / ((max - min) * (max - mode))
  } else {
    0
  }
}
dtri <- Vectorize(dtri)

#' @rdname Triangular
#' @export
qtri <- function(p, min, max, mode) {
  if (p < 0 | p > 1) {
    stop("Invalid probabilities (must be between 0 and 1).")
  } else if (p <= ptri(mode, min, max, mode)) {
    min + sqrt((max - min) * (mode - min) * p)
  } else {
    max - sqrt((max - min) * (max - mode) * (1 - p))
  }
}
qtri <- Vectorize(qtri)

#' @rdname Triangular
#' @export
rtri <- function(n, min, max, mode) {
  if (n == 0) {
    numeric(0)
  } else {
    if (length(n) > 1) {
      n <- length(n)
    }
    qtri(stats::runif(n), min, max, mode)
  }
}
