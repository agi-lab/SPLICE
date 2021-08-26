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
#' partial payment (`miRev_atP`), and the ones that do not coincide with a
#' payment (`miRev_NatP`).
#'
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param miRev_no_atP_function function of `no_pmt` (number of partial
#' payments) that generates and returns the number of minor revisions that are
#' simultaneous with a partial payment (see Details for the default function).
#' @param miRev_no_NatP_function function of `setldel` (settlement delay) that
#' generates and returns the number of minor revisions that are not
#' simultaneous with a partial payment (see Details for the default function).
#' @param frequency_vector a vector of claim frequencies for all the periods
#' (not required if the `claims` argument is provided).
#' @param settlement_list list of settlement delays (not required if the
#' `claims` argument is provided).
#' @param no_payments_list list of number of partial payments (not required if
#' the `claims` argument is provided).
#'
#' @section Details - `claim_miRev_no` (Frequency): Minor revisions may occur
#' simultaneously with a partial payment, or at any other time.
#'
#' For the former case, we sample the occurrence of minor revisions as Bernoulli
#' random variables with probability parameter \eqn{p = 1/2}.
#'
#' For the latter case, we sample the number of (non payment simultaneous) minor
#' revisions from a geometric distribution with mean =
#' \eqn{min(3, setldel / 4)}.
#'
#' One can modify the above sampling distributions by plugging in their own
#' `miRev_no_atP_function` and `miRev_no_NatP_function`, where the former
#' simulates the number of minor revisions at partial payments (and returns a
#' vector of revision indicators), and the latter simulates and returns the
#' number of minor revisions at any other points in time, with possible
#' dependence on the settlement delay of the claim.
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
  miRev_no_atP_function,
  miRev_no_NatP_function,
  frequency_vector = claims$frequency_vector,
  settlement_list = claims$settlement_list,
  no_payments_list = claims$no_payments_list
) {

  # default function to simulate the number of minor revisions
  if (missing(miRev_no_atP_function)) {
    miRev_no_atP_function <- function(no_pmt) {
      # number of minor revisions, simultaneous with partial payment
      rev_atP <- sample(c(0, 1), size = no_pmt, replace = TRUE)
      # return the revision at payment indicators
      rev_atP
    }
  }

  if (missing(miRev_no_NatP_function)) {
    miRev_no_NatP_function <- function(setldel) {
      k2 <- stats::rgeom(n = 1, prob = 1 / (min(3, setldel/4) + 1))
      k2
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
  for (i in 1:I) {
    miRev[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      miRev[[i]][[j]] <- miRev_unit
      miRev[[i]][[j]]$miRev_atP <- miRev_no_atP_function(no_payments_list[[i]][j])
      miRev[[i]][[j]]$miRev_no_atP <- sum(miRev[[i]][[j]]$miRev_atP)
      miRev[[i]][[j]]$miRev_no_NatP <- miRev_no_NatP_function(settlement_list[[i]][j])
    }
  }

  miRev
}

#' @rdname claim_miRev
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see `claims`.
#' @param miRev_list nested list of minor revision histories (with non-empty
#' revision frequencies).
#' @param miRev_time_NatP_function function of `miRev_no_NatP`, `setldel` that
#' generates and returns the epochs of minor revisions that do not occur with a
#' partial payment, measured from claim notification (see Details for the
#' default function).
#' @param settlement_list list of settlement delays (not required if the
#' `claims` argument is provided).
#' @param payment_delay_list (compound) list of inter partial delays (not
#' required if the `claims` argument is provided).
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
#' `miRev_time_NatP_function`, which simulates and returns the times of minor
#' revisions that do not coincide with a payment, with possible dependence on
#' the settlement delay of the claim (see Examples).
#'
#' @export
claim_miRev_time <- function(
  claims,
  miRev_list,
  miRev_time_NatP_function,
  settlement_list = claims$settlement_list,
  payment_delay_list = claims$payment_delay_list
) {

  # default function to simulate the timing of minor revisions (from notification)
  # for revisions non-simultaneous with payments
  if (missing(miRev_time_NatP_function)) {
    miRev_time_NatP_function <- function(miRev_no_NatP, setldel) {
      sort(stats::runif(n = miRev_no_NatP, min = setldel/6, max = setldel))
    }
  }

  I <- length(miRev_list)
  for (i in 1:I) {
    for (j in 1 : length(miRev_list[[i]])) {

      k1 <- miRev_list[[i]][[j]]$miRev_no_atP
      k2 <- miRev_list[[i]][[j]]$miRev_no_NatP
      k <- k1 + k2
      rev_atP <- miRev_list[[i]][[j]]$miRev_atP
      payment_delays <- payment_delay_list[[i]][[j]]

      # revisions that coincide with the payments
      miRev_time_atP <- cumsum(payment_delays)[as.logical(rev_atP)]
      # revisions that occur non-simultaneously with a payment
      miRev_time_NatP <- miRev_time_NatP_function(
        miRev_no_NatP = k2,
        setldel = settlement_list[[i]][j])

      miRev_list[[i]][[j]]$miRev_time_atP <- miRev_time_atP
      miRev_list[[i]][[j]]$miRev_time_NatP <- miRev_time_NatP
    }
  }

  miRev_list
}


#' @rdname claim_miRev
#' @param maRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param miRev_size_function function of `miRev_time` (vector, epochs of all
#' minor revisions), `maRev_time_2nd` (time of the second major revision),
#' `setldel` that generates and returns the sizes of the minor revision
#' multipliers (see Details for the default function).
#'
#' @section Details - `claim_miRev_size` (Revision Multiplier): The sampling
#' distribution for minor revision multipliers is the same for both revisions
#' that occur with and without a partial payment. We allow sampling dependence
#' on the delay from notification to settlement (`setldel`), the delay from
#' notification to the subject minor revisions (`miRev_time`), and the history
#' of major revisions (in particular, the time of the second major revision).
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
#' Unlike the major revision multipliers which apply to the incurred loss
#' estimates, the minor revision multipliers apply to the case estimate of
#' outstanding claim payments i.e. a revision multiplier of 2.54 means that at
#' the time of the minor revision the outstanding claims payment increases by
#' a factor of 2.54.
#'
#' @examples
#' set.seed(1)
#' minor <- claim_miRev_no(test_claims_object)
#' minor[[1]][[1]] # the "unit list" for the first claim
#'
#' # update the timing information
#' minor <- claim_miRev_time(test_claims_object, minor)
#' # observe how this has changed
#' minor[[1]][[1]]
#' # with an alternative sampling distribution e.g. triangular
#' my_miRev_time_function <- function(miRev_no_NatP, setldel) {
#'   sort(rtri(n = miRev_no_NatP, min = setldel/6, max = setldel, mode = setldel))
#' }
#' minor_2 <- claim_miRev_time(test_claims_object, minor, my_miRev_time_function)
#'
#' # update the revision multipliers (need to generate "major" first)
#' # minor <- claim_miRev_size(test_claims_object, major, minor)
#' @export
claim_miRev_size <- function(
  claims,
  maRev_list, # to get the time of the 2nd major revision
  miRev_list,
  miRev_size_function,
  settlement_list = claims$settlement_list
) {

  # default function to simulate multiplier sizes (common for miRev atP and NatP)
  if (missing(miRev_size_function)) {
    miRev_size_function <- function(
      miRev_time, maRev_time_2nd, setldel) {

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
  }

  I <- length(miRev_list)
  for (i in 1:I) {
    for (j in 1 : length(miRev_list[[i]])) {

      setldel <- settlement_list[[i]][j]
      maRev_time_2nd <- maRev_list[[i]][[j]]$maRev_time[2]
      miRev_time_atP <- miRev_list[[i]][[j]]$miRev_time_atP
      miRev_time_NatP <- miRev_list[[i]][[j]]$miRev_time_NatP
      if (is.na(maRev_time_2nd)) {
        maRev_time_2nd <- setldel + 1 # so it always holds miRev_time < maRev_time_2nd
        stopifnot(miRev_time_atP < maRev_time_2nd)
        stopifnot(miRev_time_NatP < maRev_time_2nd)
      }

      miRev_list[[i]][[j]]$miRev_multiplier_atP <- miRev_size_function(
        miRev_time = miRev_time_atP,
        maRev_time_2nd,
        setldel = setldel
      )

      miRev_list[[i]][[j]]$miRev_multiplier_NatP <- miRev_size_function(
        miRev_time = miRev_time_NatP,
        maRev_time_2nd,
        setldel = setldel
      )

    }
  }

  miRev_list
}
