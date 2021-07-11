###############################################################################
##                       9. Major Revisions - Building Blocks                ##
###############################################################################

#' Major Revisions of Incurred Loss
#'
#' A suite of functions that works together to simulate, in order, the (1)
#' frequency, (2) time, and (3) size of major revisions of incurred loss, for
#' each of the claims occurring in each of the periods.
#'
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see `claims`.
#' @param claim_size_benchmark a value below which claims are assumed to have
#' no major revisions other than one at claim notification, unless an
#' alternative `maRev_no_function` function is specified, default benchmark at
#' 0.075 * `ref_claim`.
#' @param maRev_no_function function of `claim_size` that generates and returns
#' the number of major revisions (see Details for the default function).
#' @param frequency_vector a vector of claim frequencies for all the periods
#' (not required if the `claims` argument is provided).
#' @param claim_size_list list of claim sizes (not required if the `claims`
#' argument is provided).
#'
#' @section Details - `claim_maRev_no` (Frequency): Let *K* represent the number
#' of major revisions associated with a particular claim. The notification of a
#' claim is considered as a major revision, so all claims have at least 1 major
#' revision (\eqn{K \ge 1}).
#'
#' The default `maRev_no_function` specifies that no additional major revisions
#' will occur for claims of size smaller than or equal to `claim_size_benchmark`
#' (0.075 * `ref_claim` by default). For claims above this threshold,
#' \tabular{ll}{
#' \eqn{Pr(K = 2)} \tab \eqn{= 0.1 + 0.3min(1, (claim_size - 0.075 * ref_claim) / 0.925 * ref_claim)} \cr
#' \eqn{Pr(K = 3)} \tab \eqn{= 0.5min(1, max(0, claim_size - 0.25 * ref_claim)/ (0.75 * ref_claim))} \cr
#' \eqn{Pr(K = 1)} \tab \eqn{= 1 - Pr(K = 2) - Pr(K = 3)}
#' }
#' The idea is that	major revisions are more likely for larger claims, and do
#' not occur at all for the smallest claims. Note also that by default a claim
#' may experience **up to a maximum of 2 major revisions** in addition to the
#' one at claim notification. This is taken as an assumption in the default
#' setting of `claim_maRev_size()`. If user decides to modify this assumption,
#' they will need to take care of the part on the major revision size as well.
#' @return A nested list structure such that the *j*th component of the *i*th
#' sub-list is a list of information on major revisions of the *j*th claim of
#' occurrence period *i*. The "unit list" (i.e. the smallest, innermost
#' sub-list) contains the following components:
#' \tabular{ll}{
#' `maRev_no` \tab Number of major revisions of incurred loss
#' \[`claim_maRev_no()`\]. \cr
#' `maRev_time` \tab Time of major revisions (from claim notification)
#' \[`claim_maRev_time()`\]. \cr
#' `maRev_multiplier` \tab Major revision multiplier of **incurred loss**
#' \[`claim_maRev_size()`\]. \cr
#' `maRev_atP` \tab An indicator, `1` if the last major revision occurs at the
#' time of the last major payment (i.e. second last payment), `0` otherwise
#' \[`claim_maRev_time()`\].
#' }
#' @seealso \code{\link{claims}}
#' @export
#' @name claim_maRev
claim_maRev_no <- function(
  claims,
  claim_size_benchmark = 0.075 * .pkgenv$ref_claim,
  maRev_no_function,
  frequency_vector = claims$frequency_vector,
  claim_size_list = claims$claim_size_list
) {

  # default function to simulate the number of major revisions
  # NOTE: the maRev_size_function takes as an assumption that there are max 3
  # major revisions -> if user wants to change this, they need to take care of
  # the later module (maRev_size) too
  if (missing(maRev_no_function)) {
    maRev_no_function <- function(claim_size) {
      if (claim_size <= claim_size_benchmark) {
        k <- 1
      } else {
        Pr2 <- 0.1 + 0.3 *
          min(1, (claim_size - 0.075*.pkgenv$ref_claim)/(0.925*.pkgenv$ref_claim))
        Pr3 <- 0.5 *
          min(1, max(0, claim_size - 0.25*.pkgenv$ref_claim)/(0.75*.pkgenv$ref_claim))
        Pr1 <- 1 - Pr2 - Pr3
        k <- sample(c(1, 2, 3), size = 1, replace = TRUE, prob = c(Pr1, Pr2, Pr3))
      }

      k
    }
  }

  I <- length(frequency_vector)
  maRev <- vector("list", I)
  # maRev_unit stores all major revision information on a single claim
  maRev_unit <- list(
    maRev_no = NA,
    maRev_time = NA,
    maRev_multiplier = NA,
    maRev_atP = NA
  )
  for (i in 1:I) {
    maRev[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      maRev[[i]][[j]] <- maRev_unit
      maRev[[i]][[j]]$maRev_no <- maRev_no_function(claim_size_list[[i]][j])
    }
  }

  maRev
}


#' @rdname claim_maRev
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see `claims`.
#' @param maRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param maRev_time_function function of `maRev_no`, `claim_size`, `setldel`,
#' `payment_delays` that generates and returns the epochs of major revisions
#' measured from claim notification (see Details for the default function).
#' @param claim_size_list list of claim sizes (not required if the `claims`
#' argument is provided).
#' @param settlement_list list of settlement delays (not required if the
#' `claims` argument is provided).
#' @param payment_delay_list (compound) list of inter partial delays (not
#' required if the `claims` argument is provided).
#'
#' @section Details - `claim_maRev_time` (Time): Let \eqn{\tau_k} represent the
#' epoch of the *k*th major revision (time measured from claim notification),
#' \eqn{k = 1, ..., K}. As the notification of a claim is considered a major
#' revision itself, we have \eqn{\tau_1 = 0} for all claims.
#'
#' The last major revision for a claim may occur at the time of the second last
#' partial payment (which is usually the major settlement payment) with
#' probability
#' \deqn{0.2 min(1, max(0, (claim_size - ref_claim) / (14 * ref_claim)))}
#' where `ref_claim` is a package-wise global variable that user is required to
#' define at the top of their code using `set_parameters()`.
#'
#' Now, if there is a major revision at the time of the second last partial
#' payment, then \eqn{\tau_k, k = 2, ..., K - 1} are sampled from a triangular
#' distribution with parameters (see also \code{\link{ptri}})
#' - `min = time_to_second_last_payment / 3`
#' - `max = time_to_second_last_payment`
#' - maximum density at `mode = time_to_second_last_payment / 3`.
#'
#' Otherwise (i.e. no major revision at the time of the second last partial
#' payment), \eqn{\tau_k, k = 2, ..., K} are sampled from a triangular
#' distribution with parameters
#' - `min = settlement_delay / 3`
#' - `max = settlement_delay`
#' - maximum density at `mode = settlement_delay / 3`.
#'
#' Note that when there is a major revision at the time of the second last
#' partial payment, `maRev_atP` (one of the output list components) will be set
#' to be 1.
#' @export
claim_maRev_time <- function(
  claims,
  maRev_list,
  maRev_time_function,
  claim_size_list = claims$claim_size_list,
  settlement_list = claims$settlement_list,
  payment_delay_list = claims$payment_delay_list
) {

  # default function to simulate the timing of major revisions (from notification)
  if (missing(maRev_time_function)) {
    maRev_time_function <- function(maRev_no, claim_size, setldel, payment_delays) {
      k <- maRev_no
      no_pmt <- length(payment_delays)
      maRev_time <- rep(NA, times = k)
      maRev_time[1] <- 0 # first revision at notification
      if (k > 1) {
        # if the claim has multiple major revisions
        # does the last revision occur exactly at the second last partial payment?
        p <- 0.2 *
          min(1, max(0, (claim_size - .pkgenv$ref_claim) / (14*.pkgenv$ref_claim)))
        at_second_last_pmt <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(1-p, p))
        if (at_second_last_pmt == 0) {
          # no revision at second last payment
          maRev_time[2:k] <- sort(rtri(k - 1, min = setldel/3, max = setldel, mode = setldel/3))
        } else {
          # revision at second last payment
          maRev_time[k] <- sum(payment_delays[1:(no_pmt - 1)])
          if (k > 2) {
            maRev_time[2:(k-1)] <- sort(
              rtri(k - 2, min = maRev_time[k]/3, max = maRev_time[k], mode = maRev_time[k]/3))
          }
        }
      }
      maRev_time
    }
  }

  I <- length(maRev_list)
  for (i in 1:I) {
    for (j in 1 : length(maRev_list[[i]])) {

      k <- maRev_list[[i]][[j]]$maRev_no
      payment_delays <- payment_delay_list[[i]][[j]]

      maRev_time <- maRev_time_function(
        maRev_no = k,
        claim_size = claim_size_list[[i]][j],
        setldel = settlement_list[[i]][j],
        payment_delays = payment_delays)

      maRev_list[[i]][[j]]$maRev_time <- maRev_time
      # is there a revision at second last payment?
      no_pmt <- length(payment_delays)
      maRev_list[[i]][[j]]$maRev_atP <- ifelse(
        maRev_time[k] == sum(payment_delays[1:(no_pmt - 1)]), 1, 0)
    }
  }

  maRev_list
}


#' @rdname claim_maRev
#' @param maRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param maRev_size_function function of `maRev_no` that generates and returns
#' the sizes of the major revision multipliers (see Details for the default
#' function).
#'
#' @section Details - `claim_maRev_size` (Revision Multiplier): As mentioned in
#' the frequency section ("Details - `claim_maRev_no`"), the default function
#' for the major revision multipliers assumes that there are only up to 2 major
#' revisions (in addition to the one at claim notification) for all claims.
#'
#' By default,
#' - the first major revision multiplier \eqn{g_1} is simply 1 (no meaning);
#' - the second major revision multiplier \eqn{g_2} is sampled from a lognormal
#' distribution with parameters `meanlog` \eqn{ = 1.8} and `sdlog` \eqn{= 0.2};
#' - the third major revision multiplier \eqn{g_3} is sampled from a lognormal
#' distribution with parameters `meanlog` \eqn{= 1 + 0.07(6 - g_2)} and `sdlog`
#' \eqn{= 0.1}. Note that the third major revision is likely to be smaller than
#' the second.
#'
#' The revision multipliers are subject to further constraints to ensure that
#' the revised incurred estimate never falls below what has already been paid.
#' This is dicussed in \code{\link{claim_history}}.
#'
#' The major revision multipliers apply to the incurred loss estimates, that is,
#' a revision multiplier of 2.54 means that at the time of the major revision
#' the incurred loss increases by a factor of 2.54. We highlight this as in the
#' case of minor revisions, the multiplers will instead apply to the outstanding
#' claim amounts, see \code{\link{claim_miRev}}.
#'
#' @examples
#' set.seed(1)
#' major <- claim_maRev_no(test_claims_object)
#' major[[1]][[1]] # the "unit list" for the first claim
#'
#' # update the timing information
#' major <- claim_maRev_time(test_claims_object, major)
#' # observe how this has changed
#' major[[1]][[1]]
#'
#' # update the revision multipliers
#' major <- claim_maRev_size(major)
#' # again observe how this has changed
#' major[[1]][[1]]
#' @export
claim_maRev_size <- function(
  maRev_list,
  maRev_size_function
) {

  # default function to simulate multiplier sizes
  # NOTE: this only works for up to 3 major revisions -> if user has adjusted
  # the maRev_no_function, they will need to adjust this accordingly
  if (missing(maRev_size_function)) {
    maRev_size_function <- function(maRev_no) {
      maRev_multiplier <- rep(NA, times = maRev_no)
      maRev_multiplier[1] <- 1 # for first revision (at notification)
      if (maRev_no > 1) { # if the claim has multiple major revisions
        maRev_multiplier[2] <- stats::rlnorm(n = 1, meanlog = 1.8, sdlog = 0.2)
        if (maRev_no > 2) {
          mu <- 1 + 0.07 * (6 - maRev_multiplier[2])
          maRev_multiplier[3] <- stats::rlnorm(n = 1, meanlog = mu, sdlog = 0.1)
        }
      }

      maRev_multiplier
    }
  }

  I <- length(maRev_list)
  for (i in 1:I) {
    for (j in 1 : length(maRev_list[[i]])) {
      k <- maRev_list[[i]][[j]]$maRev_no
      maRev_list[[i]][[j]]$maRev_multiplier <- maRev_size_function(maRev_no = k)
    }
  }

  maRev_list

}
