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
#' payment (denoted `_NatP`).
#'
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param prob_atP (optional) probability that a minor revision will occur at
#' the time of a partial payment; default value 0.5.
#' @param rfun_NatP optional alternative random sampling function for:
#' * `claim_miRev_no`: the number of minor revisions that occur at an epoch
#' other than those of partial payments;
#' * `claim_miRev_time`: the epochs of such minor revisions measured from claim
#' notification;
#' * `claim_miRev_size`: the sizes of the minor revision multipliers (common for
#' `_atP` and `_NatP`, hence simply termed `rfun` in this case).
#'
#' See Details for default.
#' @param paramfun_NatP parameters for the above random sampling function,
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
#' @section Details - `claim_miRev_no` (Frequency): Minor revisions may occur
#' simultaneously with a partial payment, or at any other time.
#'
#' For the former case, we sample the occurrence of minor revisions as Bernoulli
#' random variables with default probability parameter \eqn{p = 1/2}.
#'
#' For the latter case, by default we sample the number of (non payment
#' simultaneous) minor revisions from a geometric distribution with mean =
#' \eqn{min(3, setldel / 4)}.
#'
#' One can modify the above sampling distributions by plugging in their own
#' `prob_atP` parameter and `rfun_NatP` function, where the former dictates
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
#' `miRev_atP` \tab A vector of indicators showing whether there is a minor
#' revision at each partial payment \[`claim_miRev_no()`\]. \cr
#' `miRev_no_atP` \tab Number of minor revisions that occur simultaneously with
#' a partial payment, numerically equals to the sum of `miRev_atP`
#' \[`claim_miRev_no()`\]. \cr
#' `miRev_no_NatP` \tab Number of minor revisions that do not occur with a
#' partial payment \[`claim_miRev_no()`\]. \cr
#' `miRev_time_atP` \tab Time of minor revisions that occur simultaneously with
#' a partial payment (time measured from claim notification)
#' \[`claim_miRev_time()`\]. \cr
#' `miRev_time_NatP` \tab Time of minor revisions that do *not* occur
#' simultaneously with a partial payment (time measured from claim notification)
#' \[`claim_miRev_time()`\]. \cr
#' `miRev_multiplier_atP` \tab Minor revision multipliers of **outstanding claim
#' payments** for revisions at partial payments \[`claim_miRev_size()`\]. \cr
#' `miRev_multiplier_NatP` \tab Minor revision multipliers of **outstanding claim
#' payments** for revisions at any other times \[`claim_miRev_size()`\]. \cr
#' }
#' @seealso \code{\link[SynthETIC]{claims}}
#' @export
#' @name claim_miRev
claim_miRev_no <- function(
  claims,
  prob_atP = 0.5, # probability of coinciding with a partial payment
  rfun_NatP,      # miRev_no_NatP_function
  paramfun_NatP,  # paramfun for miRev_no_NatP (not at payment)
  frequency_vector = claims$frequency_vector,
  settlement_list = claims$settlement_list,
  no_payments_list = claims$no_payments_list,
  ...
) {

  if (!missing(rfun_NatP) & missing(paramfun_NatP)) {
    # we will see if we can continue without parameterisation
    paramfun_NatP <- function(...) {
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
  if (missing(rfun_NatP)) {
    rfun_NatP <- function(n, setldel) {
      # n = number of observations/claims
      k2 <- stats::rgeom(n, prob = 1 / (min(3, setldel/4) + 1))
      k2
    }

    # the default rfun directly takes setldel as an input, so the "empty"
    # paramfun would do the trick
    paramfun_NatP <- function(...) {
      c(...)
    }
  }

  I <- length(frequency_vector)
  miRev <- vector("list", I)
  # miRev_unit stores all minor revision information on a single claim
  miRev_unit <- list(
    miRev_atP = NA,
    miRev_no_atP = NA, miRev_no_NatP = NA,
    miRev_time_atP = NA, miRev_time_NatP = NA,
    miRev_multiplier_atP = NA, miRev_multiplier_NatP = NA
  )
  # set up the simulation parameters
  params <- mapply(paramfun_NatP,
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

  # do.call rfun_NatP, but ignore unused arguments
  args <- as.list(params_split)
  keep_names <- c(intersect(names(args), names(formals(rfun_NatP))))
  keep_formals <- c(args[keep_names])

  # turn keep_formals, which is a list of arguments, to a dataframe
  args_df <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  curr <- 1
  for (i in 1:I) {
    miRev[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      miRev[[i]][[j]] <- miRev_unit

      # miRev simultaneous with a payment
      miRev[[i]][[j]]$miRev_atP <- rfun_atP(
        n = no_payments_list[[i]][j], prob = prob_atP)
      miRev[[i]][[j]]$miRev_no_atP <- sum(miRev[[i]][[j]]$miRev_atP)

      # miRev non-simultaneous with a payment
      if (paramfun_filled) {
        tt <- try(miRev[[i]][[j]]$miRev_no_NatP <- do.call(
          rfun_NatP, c(as.list(args_df[, curr]), n = 1)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun_NatP' for the sampling distribution")
        }
      } else {
        miRev[[i]][[j]]$miRev_no_NatP <- do.call(
          rfun_NatP, c(as.list(args_df[, curr]), n = 1))
      }

      curr <- curr + 1
    }
  }

  miRev
}

#' @rdname claim_miRev
#' @param miRev_list nested list of minor revision histories (with non-empty
#' revision frequencies).
#' @param payment_delay_list (compound) list of inter partial delays (not
#' required if the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_payment_delay}}.
#'
#' @section Details - `claim_miRev_time` (Time): For minor revisions that occur
#' simultaneously with a partial payment, the revision times simply coincide
#' with the epochs of the relevant partial payments.
#'
#' For minor revisions that occur at a different time, by default the revision
#' times are sampled from a uniform distribution with parameters `min` \eqn{=
#' settlement_delay / 6} and `max` \eqn{= settlement_delay}.
#'
#' One can modify the above sampling distribution by plugging in their own
#' `rfun_NatP` and `paramfun_NatP` in `claim_miRev_time()`, which together
#' simulate the epochs of minor revisions that do not coincide with a payment,
#' with possible dependence on the settlement delay of the claim and/or other
#' claim characteristics (see Examples).
#'
#' @export
claim_miRev_time <- function(
  claims,
  miRev_list,
  rfun_NatP,
  paramfun_NatP,
  settlement_list = claims$settlement_list,
  payment_delay_list = claims$payment_delay_list,
  ...
) {

  if (!missing(rfun_NatP) & missing(paramfun_NatP)) {
    # we will see if we can continue without parameterisation
    paramfun_NatP <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default function to simulate the timing of minor revisions (from notification)
  # for revisions non-simultaneous with payments
  if (missing(rfun_NatP)) {
    rfun_NatP <- function(n, setldel) {
      # n = number of minor revisions non-simultaneous with payments
      sort(stats::runif(n, min = setldel/6, max = setldel))
    }

    # the default rfun directly takes setldel as an input, so the "empty"
    # paramfun would do the trick
    paramfun_NatP <- function(...) {
      c(...)
    }
  }

  I <- length(miRev_list)
  params <- mapply(
    paramfun_NatP,
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
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(args[keep_names])

  # turn keep_formals, which is a list of arguments, to a dataframe
  args_df <- do.call(rbind, keep_formals)
  # in the dataframe, each row represents a parameter, and each column gives the
  # parameter values for a specific claim

  curr <- 1
  for (i in 1:I) {
    for (j in 1 : length(miRev_list[[i]])) {

      k1 <- miRev_list[[i]][[j]]$miRev_no_atP
      k2 <- miRev_list[[i]][[j]]$miRev_no_NatP
      k <- k1 + k2
      rev_atP <- miRev_list[[i]][[j]]$miRev_atP
      payment_delays <- payment_delay_list[[i]][[j]]

      # revisions that coincide with the payments
      miRev_list[[i]][[j]]$miRev_time_atP <-
        cumsum(payment_delays)[as.logical(rev_atP)]
      # revisions that occur non-simultaneously with a payment
      if (paramfun_filled) {
        tt <- try(miRev_list[[i]][[j]]$miRev_time_NatP <- do.call(
          rfun_NatP, c(as.list(args_df[, curr]), n = k2)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun_NatP' for the sampling distribution")
        }
      } else {
        miRev_list[[i]][[j]]$miRev_time_NatP <- do.call(
          rfun_NatP, c(as.list(args_df[, curr]), n = k2))
      }

      curr <- curr + 1
    }
  }

  miRev_list
}


#' @rdname claim_miRev
#' @param maRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#'
#' @section Details - `claim_miRev_size` (Revision Multiplier): The sampling
#' distribution for minor revision multipliers is the same for both revisions
#' that occur with and without a partial payment. In the default setting, we
#' incorporate sampling dependence on the delay from notification to settlement
#' (`setldel`), the delay from notification to the subject minor revisions
#' (`miRev_time`), and the history of major revisions (in particular, the time
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
#' minor <- claim_miRev_no(test_claims)
#' minor[[1]][[1]] # the "unit list" for the first claim
#'
#' # update the timing information
#' minor <- claim_miRev_time(test_claims, minor)
#' # observe how this has changed
#' minor[[1]][[1]]
#' # with an alternative sampling distribution e.g. triangular
#' miRev_time_NatP <- function(n, setldel) {
#'   sort(rtri(n, min = setldel/6, max = setldel, mode = setldel))
#' }
#' minor_2 <- claim_miRev_time(test_claims, minor, miRev_time_NatP)
#'
#' # update the revision multipliers (need to generate "major" first)
#' # minor <- claim_miRev_size(test_claims, major, minor)
#' @export
claim_miRev_size <- function(
  claims,
  maRev_list, # to get the time of the 2nd major revision
  miRev_list,
  rfun,
  paramfun_atP,
  paramfun_NatP,
  settlement_list = claims$settlement_list,
  ...
) {

  if (!missing(rfun) & (missing(paramfun_atP) | missing(paramfun_NatP))) {
    stop("need to specify 'paramfun' for the sampling distribution")
  }

  # default function to simulate multiplier sizes (common for miRev atP and NatP)
  if (missing(rfun)) {
    rfun <- function(
      # n = number of minor revisions
      n, miRev_time, maRev_time_2nd, setldel) {

      k <- length(miRev_time)
      miRev_multiplier <- vector(length = k)

      if (k >= 1) {
        for (i in 1:k) {
          curr <- miRev_time[i]
          if (curr <= setldel/3) {
            if (curr > maRev_time_2nd) {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = 0.15, sdlog = 0.05)
            } else {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = 0.15, sdlog = 0.1)
            }
          } else if (curr <= (2/3) * setldel) {
            if (curr > maRev_time_2nd) {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = 0, sdlog = 0.05)
            } else {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = 0, sdlog = 0.1)
            }
          } else {
            if (curr > maRev_time_2nd) {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = -0.1, sdlog = 0.05)
            } else {
              miRev_multiplier[i] <- stats::rlnorm(n = 1, meanlog = -0.1, sdlog = 0.1)
            }
          }
        }
      }

      miRev_multiplier
    }

    # extract miRev_time and maRev_time_2nd
    paramfun_atP <- function(major, minor, setldel, ...) {
      list(miRev_time = minor$miRev_time_atP,
           maRev_time_2nd = ifelse(
             # so it always holds miRev_time < maRev_time_2nd
             is.na(major$maRev_time[2]), setldel + 1, major$maRev_time[2]),
           setldel = setldel,
           ...)
    }

    paramfun_NatP <- function(major, minor, setldel, ...) {
      list(miRev_time = minor$miRev_time_NatP,
           maRev_time_2nd = ifelse(
             # so it always holds miRev_time < maRev_time_2nd
             is.na(major$maRev_time[2]), setldel + 1, major$maRev_time[2]),
           setldel = setldel,
           ...)
    }
  }

  I <- length(miRev_list)
  params_atP <- mapply(
    paramfun_atP,
    setldel = unlist(settlement_list, use.names = FALSE),
    major = unlist(maRev_list, use.names = FALSE, recursive = FALSE),
    minor = unlist(miRev_list, use.names = FALSE, recursive = FALSE),
    ...
  )
  params_NatP <- mapply(
    paramfun_NatP,
    setldel = unlist(settlement_list, use.names = FALSE),
    major = unlist(maRev_list, use.names = FALSE, recursive = FALSE),
    minor = unlist(miRev_list, use.names = FALSE, recursive = FALSE),
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

  # repeat for params_NatP
  if (!is.null(names(params_NatP))) {
    params_split_NatP <- split(unname(params_NatP), names(params_NatP))
  } else if (length(params_NatP)) {
    params_split_NatP <- asplit(params_NatP, 1)
  } else {
    params_split_NatP <- params_NatP
  }
  args <- as.list(params_split_NatP)
  keep_names <- c(intersect(names(args), names(formals(rfun))))
  keep_formals <- c(args[keep_names])
  args_df_NatP <- do.call(rbind, keep_formals)

  curr <- 1
  for (i in 1:I) {
    for (j in 1 : length(miRev_list[[i]])) {

      k1 <- miRev_list[[i]][[j]]$miRev_no_atP
      k2 <- miRev_list[[i]][[j]]$miRev_no_NatP

      miRev_list[[i]][[j]]$miRev_multiplier_atP <- do.call(
        rfun, c(as.list(args_df_atP[, curr]), n = k1))
      miRev_list[[i]][[j]]$miRev_multiplier_NatP <- do.call(
        rfun, c(as.list(args_df_NatP[, curr]), n = k2))

      curr <- curr + 1
    }
  }

  miRev_list
}
