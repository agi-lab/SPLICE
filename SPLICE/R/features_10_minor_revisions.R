###############################################################################
##                      10. Minor Revisions - Building Blocks                ##
###############################################################################

#' Minor Revisions of Outstanding Claim Payments
#'
#' @description  A suite of functions that works together to simulate, in order,
#' the (1) frequency, (2) time, and (3) size of minor revisions of outstanding
#' claim payments, for each of the claims occurring in each of the periods.
#'
#' We separate the case of minor revisions that occur simultaneously with a
#' partial payment (denoted `_atP`), and the ones that do not coincide with a
#' payment (denoted `_notatP`).
#'
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param prob_atP (optional) probability that a minor revision will occur at
#' the time of a partial payment; default value 0.5.
#' @param rfun_notatP optional alternative random sampling function for:
#' * `claim_minRev_freq`: the number of minor revisions that occur at an epoch
#' other than those of partial payments;
#' * `claim_minRev_time`: the epochs of such minor revisions measured from claim
#' notification;
#' * `claim_minRev_size`: the sizes of the minor revision multipliers (common for
#' `_atP` and `_notatP`, hence simply termed `rfun` in this case).
#'
#' See Details for default.
#' @param paramfun_notatP parameters for the above random sampling function,
#' as a function of other claim characteristics (e.g. `lambda` as a function of
#' `claim_size` for an `rpois` simulation); see Examples.
#' @param frequency_vector a vector of claim frequencies for all the periods
#' (not required if the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_frequency}}.
#' @param settlement_list list of settlement delays (not required if the
#' `claims` argument is provided); see \code{\link[SynthETIC]{claim_closure}}.
#' @param no_payments_list list of number of partial payments (not required if
#' the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_payment_no}}.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @section Details - `claim_minRev_freq` (Frequency): Minor revisions may occur
#' simultaneously with a partial payment, or at any other time.
#'
#' For the former case, we sample the occurrence of minor revisions as Bernoulli
#' random variables with default probability parameter `prob_atP` \eqn{= 1/2}.
#'
#' For the latter case, by default we sample the number of (non payment
#' simultaneous) minor revisions from a geometric distribution with mean =
#' \eqn{min(3, setldel / 4)}.
#'
#' One can modify the above sampling distributions by plugging in their own
#' `prob_atP` parameter and `rfun_notatP` function, where the former dictates
#' the probability of incurring a minor revision at the time of a payment, and
#' the latter simulates and returns the number of minor revisions at any other
#' points in time, with possible dependence on the settlement delay of the claim
#' and/or other claim characteristics.
#'
#' @return A nested list structure such that the *j*th component of the *i*th
#' sub-list is a list of information on minor revisions of the *j*th claim of
#' occurrence period *i*. The "unit list" (i.e. the smallest, innermost
#' sub-list) contains the following components:
#' \tabular{ll}{
#' `minRev_atP` \tab A vector of indicators showing whether there is a minor
#' revision at each partial payment \[`claim_minRev_freq()`\]. \cr
#' `minRev_freq_atP` \tab Number of minor revisions that occur simultaneously with
#' a partial payment, numerically equals to the sum of `minRev_atP`
#' \[`claim_minRev_freq()`\]. \cr
#' `minRev_freq_notatP` \tab Number of minor revisions that do not occur with a
#' partial payment \[`claim_minRev_freq()`\]. \cr
#' `minRev_time_atP` \tab Time of minor revisions that occur simultaneously with
#' a partial payment (time measured from claim notification)
#' \[`claim_minRev_time()`\]. \cr
#' `minRev_time_notatP` \tab Time of minor revisions that do *not* occur
#' simultaneously with a partial payment (time measured from claim notification)
#' \[`claim_minRev_time()`\]. \cr
#' `minRev_factor_atP` \tab Minor revision multipliers of **outstanding claim
#' payments** for revisions at partial payments \[`claim_minRev_size()`\]. \cr
#' `minRev_factor_notatP` \tab Minor revision multipliers of **outstanding claim
#' payments** for revisions at any other times \[`claim_minRev_size()`\]. \cr
#' }
#' @seealso \code{\link[SynthETIC]{claims}}
#' @export
#' @name claim_minRev
claim_minRev_freq <- function(
  claims,
  prob_atP = 0.5, # probability of coinciding with a partial payment
  rfun_notatP,      # minRev_freq_notatP_function
  paramfun_notatP,  # paramfun for minRev_freq_notatP (not at payment)
  frequency_vector = claims$frequency_vector,
  settlement_list = claims$settlement_list,
  no_payments_list = claims$no_payments_list,
  ...
) {

  if (!missing(rfun_notatP) & missing(paramfun_notatP)) {
    # we will see if we can continue without parameterisation
    paramfun_notatP <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # function to simulate the number of minor revisions
  # ... that coincide with a payment
  rfun_atP <- function(n, prob) {
    # n = number of payments
    if (prob == 0.5) {
      rev_atP <- sample(c(0, 1), size = n, replace = T)
    } else {
      rev_atP <- sample(c(0, 1), size = n, replace = T, prob = c(1 - prob, prob))
    }
    # return the revision at payment indicators
    rev_atP
  }

  # default function to simulate the number of minor revisions
  # ... that are not simultaneous with partial payment
  if (missing(rfun_notatP)) {
    rfun_notatP <- function(n, setldel) {
      # n = number of observations/claims
      k2 <- stats::rgeom(n, prob = 1 / (min(3, setldel/4) + 1))
      k2
    }

    # the default rfun directly takes setldel as an input, so the "empty"
    # paramfun would do the trick
    paramfun_notatP <- function(...) {
      c(...)
    }
  }

  I <- length(frequency_vector)
  minRev <- vector("list", I)
  # minRev_unit stores all minor revision information on a single claim
  minRev_unit <- list(
    minRev_atP = NA,
    minRev_freq_atP = NA, minRev_freq_notatP = NA,
    minRev_time_atP = NA, minRev_time_notatP = NA,
    minRev_factor_atP = NA, minRev_factor_notatP = NA
  )
  # set up the simulation parameters
  params <- mapply(paramfun_notatP,
                   setldel = unlist(settlement_list, use.names = FALSE),
                   ...)
  # convert to function arguments
  # if params only has one parameter, asplit() won't work
  if (!is.null(names(params))) {
    params_split <- split(unname(params), names(params))
  } else if (length(params)) {
    params_split <- asplit(params, 1)
  } else {
    params_split <- params
  }

  # do.call rfun_notatP, but ignore unused arguments
  args <- as.list(params_split)
  keep_names <- c(intersect(names(args), names(formals(rfun_notatP))))
  keep_formals <- c(args[keep_names])

  # turn keep_formals, which is a list of arguments, to a dataframe
  args_df <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  curr <- 1
  for (i in 1:I) {
    minRev[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      minRev[[i]][[j]] <- minRev_unit

      # minRev simultaneous with a payment
      minRev[[i]][[j]]$minRev_atP <- rfun_atP(
        n = no_payments_list[[i]][j], prob = prob_atP)
      minRev[[i]][[j]]$minRev_freq_atP <- sum(minRev[[i]][[j]]$minRev_atP)

      # minRev non-simultaneous with a payment
      if (paramfun_filled) {
        tt <- try(minRev[[i]][[j]]$minRev_freq_notatP <- do.call(
          rfun_notatP, c(as.list(args_df[, curr]), n = 1)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun_notatP' for the sampling distribution")
        }
      } else {
        minRev[[i]][[j]]$minRev_freq_notatP <- do.call(
          rfun_notatP, c(as.list(args_df[, curr]), n = 1))
      }

      curr <- curr + 1
    }
  }

  minRev
}

#' @rdname claim_minRev
#' @param minRev_list nested list of minor revision histories (with non-empty
#' revision frequencies).
#' @param payment_delay_list (compound) list of inter partial delays (not
#' required if the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_payment_delay}}.
#'
#' @section Details - `claim_minRev_time` (Time): For minor revisions that occur
#' simultaneously with a partial payment, the revision times simply coincide
#' with the epochs of the relevant partial payments.
#'
#' For minor revisions that occur at a different time, by default the revision
#' times are sampled from a uniform distribution with parameters `min` \eqn{=
#' settlement_delay / 6} and `max` \eqn{= settlement_delay}.
#'
#' One can modify the above sampling distribution by plugging in their own
#' `rfun_notatP` and `paramfun_notatP` in `claim_minRev_time()`, which together
#' simulate the epochs of minor revisions that do not coincide with a payment,
#' with possible dependence on the settlement delay of the claim and/or other
#' claim characteristics (see Examples).
#'
#' @export
claim_minRev_time <- function(
  claims,
  minRev_list,
  rfun_notatP,
  paramfun_notatP,
  settlement_list = claims$settlement_list,
  payment_delay_list = claims$payment_delay_list,
  ...
) {

  if (!missing(rfun_notatP) & missing(paramfun_notatP)) {
    # we will see if we can continue without parameterisation
    paramfun_notatP <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default function to simulate the timing of minor revisions (from notification)
  # for revisions non-simultaneous with payments
  if (missing(rfun_notatP)) {
    rfun_notatP <- function(n, setldel) {
      # n = number of minor revisions non-simultaneous with payments
      sort(stats::runif(n, min = setldel/6, max = setldel))
    }

    # the default rfun directly takes setldel as an input, so the "empty"
    # paramfun would do the trick
    paramfun_notatP <- function(...) {
      c(...)
    }
  }

  I <- length(minRev_list)
  params <- mapply(
    paramfun_notatP,
    setldel = unlist(settlement_list, use.names = FALSE),
    ...)

  # if params only has one parameter, asplit() won't work
  if (!is.null(names(params))) {
    params_split <- split(unname(params), names(params))
  } else if (length(params)) {
    params_split <- asplit(params, 1)
  } else {
    params_split <- params
  }

  # do.call rfun, but ignore unused arguments
  args <- as.list(params_split)
  keep_names <- c(intersect(names(args), names(formals(rfun_notatP))))
  keep_formals <- c(args[keep_names])

  # turn keep_formals, which is a list of arguments, to a dataframe
  args_df <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  curr <- 1
  for (i in 1:I) {
    for (j in 1 : length(minRev_list[[i]])) {

      k1 <- minRev_list[[i]][[j]]$minRev_freq_atP
      k2 <- minRev_list[[i]][[j]]$minRev_freq_notatP
      k <- k1 + k2
      rev_atP <- minRev_list[[i]][[j]]$minRev_atP
      payment_delays <- payment_delay_list[[i]][[j]]

      # revisions that coincide with the payments
      minRev_list[[i]][[j]]$minRev_time_atP <-
        cumsum(payment_delays)[as.logical(rev_atP)]
      # revisions that occur non-simultaneously with a payment
      if (paramfun_filled) {
        tt <- try(minRev_list[[i]][[j]]$minRev_time_notatP <- do.call(
          rfun_notatP, c(as.list(args_df[, curr]), n = k2)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun_notatP' for the sampling distribution")
        }
      } else {
        minRev_list[[i]][[j]]$minRev_time_notatP <- do.call(
          rfun_notatP, c(as.list(args_df[, curr]), n = k2))
      }

      curr <- curr + 1
    }
  }

  minRev_list
}


#' @rdname claim_minRev
#' @param majRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param rfun optional alternative random sampling function for the sizes of
#' the minor revision multipliers (common for `_atP` and `_notatP`, hence simply
#' termed `rfun` in this case).
#' @param paramfun_atP parameters for `rfun` in `claim_minRev_size()` for minor
#' revisions that occur at the time of a partial payment.
#'
#' @section Details - `claim_minRev_size` (Revision Multiplier): The sampling
#' distribution for minor revision multipliers is the same for both revisions
#' that occur with and without a partial payment. In the default setting, we
#' incorporate sampling dependence on the delay from notification to settlement
#' (`setldel`), the delay from notification to the subject minor revisions
#' (`minRev_time`), and the history of major revisions (in particular, the time
#' of the second major revision).
#'
#' Let \eqn{\tau} denote the delay from notification to the epoch of the minor
#' revision, and \eqn{w} the settlement delay. Then
#' - For \eqn{\tau \le w / 3}, the revision multiplier is sampled from a
#' lognormal distribution with parameters `meanlog` \eqn{= 0.15} and `sdlog`
#' \eqn{= 0.05} if preceded by a 2nd major revision, `sdlog` \eqn{= 0.1}
#' otherwise;
#' - For \eqn{w / 3 < \tau \le 2w / 3}, the revision multiplier is sampled
#' from a lognormal distribution with parameters `meanlog` \eqn{= 0} and `sdlog`
#' \eqn{= 0.05} if preceded by a 2nd major revision, `sdlog` \eqn{= 0.1}
#' otherwise;
#' - For \eqn{\tau > 2w / 3}, the revision multiplier is sampled from a
#' lognormal distribution with parameters `meanlog` \eqn{= -0.1} and `sdlog`
#' \eqn{= 0.05} if preceded by a 2nd major revision, `sdlog` \eqn{= 0.1}
#' otherwise.
#'
#' Note that minor revisions tend to be upward in the early part of a claim’s
#' life, and downward in the latter part.
#'
#' The revision multipliers are subject to further constraints to ensure that
#' the revised incurred estimate never falls below what has already been paid.
#' This is dicussed in \code{\link{claim_history}}.
#'
#' **Important note:** Unlike the major revision multipliers which apply to the
#' **incurred loss estimates**, the minor revision multipliers apply to the case
#' estimate of **outstanding claim payments** i.e. a revision multiplier of 2.54
#' means that at the time of the minor revision the outstanding claims payment
#' increases by a factor of 2.54.
#'
#' @examples
#' set.seed(1)
#' test_claims <- SynthETIC::test_claims_object
#'
#' # generate major revisions (required for the simulation of minor revisions)
#' major <- claim_majRev_freq(test_claims)
#' major <- claim_majRev_time(test_claims, major)
#' major <- claim_majRev_size(major)
#'
#' # generate frequency of minor revisions
#' minor <- claim_minRev_freq(test_claims)
#' minor[[1]][[1]] # the "unit list" for the first claim
#'
#' # update the timing information
#' minor <- claim_minRev_time(test_claims, minor)
#' # observe how this has changed
#' minor[[1]][[1]]
#' # with an alternative sampling distribution e.g. triangular
#' minRev_time_notatP <- function(n, setldel) {
#'   sort(rtri(n, min = setldel/6, max = setldel, mode = setldel))
#' }
#' minor_2 <- claim_minRev_time(test_claims, minor, minRev_time_notatP)
#'
#' # update the revision multipliers (need to generate "major" first)
#' minor <- claim_minRev_size(test_claims, major, minor)
#' @export
claim_minRev_size <- function(
  claims,
  majRev_list, # to get the time of the 2nd major revision
  minRev_list,
  rfun,
  paramfun_atP,
  paramfun_notatP,
  settlement_list = claims$settlement_list,
  ...
) {

  if (!missing(rfun) & (missing(paramfun_atP) | missing(paramfun_notatP))) {
    stop("need to specify 'paramfun' for the sampling distribution")
  }

  # default function to simulate multiplier sizes (common for minRev atP and NatP)
  if (missing(rfun)) {
    rfun <- function(
      # n = number of minor revisions
      n, minRev_time, majRev_time_2nd, setldel) {

      k <- length(minRev_time)
      minRev_factor <- vector(length = k)

      if (k >= 1) {
        for (i in 1:k) {
          curr <- minRev_time[i]
          if (curr <= setldel/3) {
            meanlog <- 0.15
          } else if (curr <= (2/3) * setldel) {
            meanlog <- 0
          } else {
            meanlog <- -0.1
          }
          sdlog <- ifelse(curr > majRev_time_2nd, 0.05, 0.1)
          minRev_factor[i] <- stats::rlnorm(n = 1, meanlog, sdlog)
        }
      }

      minRev_factor
    }

    # extract minRev_time and majRev_time_2nd
    paramfun_atP <- function(major, minor, setldel, ...) {
      list(minRev_time = minor$minRev_time_atP,
           majRev_time_2nd = ifelse(
             # so it always holds minRev_time < majRev_time_2nd
             is.na(major$majRev_time[2]), setldel + 1, major$majRev_time[2]),
           setldel = setldel,
           ...)
    }

    paramfun_notatP <- function(major, minor, setldel, ...) {
      list(minRev_time = minor$minRev_time_notatP,
           majRev_time_2nd = ifelse(
             # so it always holds minRev_time < majRev_time_2nd
             is.na(major$majRev_time[2]), setldel + 1, major$majRev_time[2]),
           setldel = setldel,
           ...)
    }
  }

  I <- length(minRev_list)
  params_atP <- mapply(
    paramfun_atP,
    setldel = unlist(settlement_list, use.names = FALSE),
    major = unlist(majRev_list, use.names = FALSE, recursive = FALSE),
    minor = unlist(minRev_list, use.names = FALSE, recursive = FALSE),
    ...
  )
  params_notatP <- mapply(
    paramfun_notatP,
    setldel = unlist(settlement_list, use.names = FALSE),
    major = unlist(majRev_list, use.names = FALSE, recursive = FALSE),
    minor = unlist(minRev_list, use.names = FALSE, recursive = FALSE),
    ...
  )

  # if params only has one parameter, asplit() won't work
  if (!is.null(names(params_atP))) {
    params_split_atP <- split(unname(params_atP), names(params_atP))
  } else if (length(params_atP)) {
    params_split_atP <- asplit(params_atP, 1)
  } else {
    params_split_atP <- params_atP
  }
  # do.call rfun, but ignore unused arguments
  args <- as.list(params_split_atP)
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(args[keep_names])
  args_df_atP <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  # repeat for params_notatP
  if (!is.null(names(params_notatP))) {
    params_split_notatP <- split(unname(params_notatP), names(params_notatP))
  } else if (length(params_notatP)) {
    params_split_notatP <- asplit(params_notatP, 1)
  } else {
    params_split_notatP <- params_notatP
  }
  args <- as.list(params_split_notatP)
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(args[keep_names])
  args_df_notatP <- do.call(rbind, keep_formals)

  curr <- 1
  for (i in 1:I) {
    for (j in 1 : length(minRev_list[[i]])) {

      k1 <- minRev_list[[i]][[j]]$minRev_freq_atP
      k2 <- minRev_list[[i]][[j]]$minRev_freq_notatP

      minRev_list[[i]][[j]]$minRev_factor_atP <- do.call(
        rfun, c(as.list(args_df_atP[, curr]), n = k1))
      minRev_list[[i]][[j]]$minRev_factor_notatP <- do.call(
        rfun, c(as.list(args_df_notatP[, curr]), n = k2))

      curr <- curr + 1
    }
  }

  minRev_list
}
