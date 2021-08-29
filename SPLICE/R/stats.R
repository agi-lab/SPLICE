#############################################################################
##                     Statistical Helper Functions                        ##
#############################################################################

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
#' @name triangular
NULL

#' @rdname triangular
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

#' @rdname triangular
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

#' @rdname triangular
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

#' @rdname triangular
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
