###############################################################################
##                       9. Major Revisions - Building Blocks                ##
###############################################################################

#' Major Revisions of Incurred Loss
#'
#' A suite of functions that works together to simulate, in order, the (1)
#' frequency, (2) time, and (3) size of major revisions of incurred loss, for
#' each of the claims occurring in each of the periods.
#'
#' @param claims an \code{\link[SynthETIC]{claims}} object containing all the
#' simulated quantities (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param rfun optional alternative random sampling function for:
#' * `claim_majRev_freq`: the number of major revisions;
#' * `claim_majRev_time`: the epochs of major revisions measured from claim
#' notification;
#' * `claim_majRev_size`: the sizes of the major revision multipliers.
#'
#' See Details for default.
#' @param paramfun parameters for the random sampling function, as a function of
#' other claim characteristics such as \code{claim_size}; see Details.
#' @param frequency_vector a vector of claim frequencies for all the periods
#' (not required if the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_frequency}}.
#' @param claim_size_list list of claim sizes (not required if the `claims`
#' argument is provided); see \code{\link[SynthETIC]{claim_size}}.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' For example, if going with the default sampling distribution for
#' `claim_majRev_freq`, you can specify a `claim_size_benchmark` (below which
#' claims are assumed to have no major revisions other than one at claim
#' notification; default benchmark at 0.075 * `ref_claim`).
#'
#' @section Details - `claim_majRev_freq` (Frequency): Let *K* represent the number
#' of major revisions associated with a particular claim. The notification of a
#' claim is considered as a major revision, so all claims have at least 1 major
#' revision (\eqn{K \ge 1}).
#'
#' The default `majRev_freq_function` specifies that no additional major revisions
#' will occur for claims of size smaller than or equal to `claim_size_benchmark`
#' (0.075 * `ref_claim` by default). For claims above this threshold,
#' \tabular{ll}{
#' \eqn{Pr(K = 2)} \tab \eqn{= 0.1 + 0.3min(1, (claim_size - 0.075 * ref_claim) / 0.925 * ref_claim)} \cr
#' \eqn{Pr(K = 3)} \tab \eqn{= 0.5min(1, max(0, claim_size - 0.25 * ref_claim)/ (0.75 * ref_claim))} \cr
#' \eqn{Pr(K = 1)} \tab \eqn{= 1 - Pr(K = 2) - Pr(K = 3)}
#' }
#' where `ref_claim` is a package-wise global variable that user should define
#' by \code{\link[SynthETIC]{set_parameters}} (if moving away from the default).
#'
#' The idea is that	major revisions are more likely for larger claims, and do
#' not occur at all for the smallest claims. Note also that by default a claim
#' may experience **up to a maximum of 2 major revisions** in addition to the
#' one at claim notification. This is taken as an assumption in the default
#' setting of `claim_majRev_size()`. If user decides to modify this assumption,
#' they will need to take care of the part on the major revision size as well.
#' @return A nested list structure such that the *j*th component of the *i*th
#' sub-list is a list of information on major revisions of the *j*th claim of
#' occurrence period *i*. The "unit list" (i.e. the smallest, innermost
#' sub-list) contains the following components:
#' \tabular{ll}{
#' `majRev_freq` \tab Number of major revisions of incurred loss
#' \[`claim_majRev_freq()`\]. \cr
#' `majRev_time` \tab Time of major revisions (from claim notification)
#' \[`claim_majRev_time()`\]. \cr
#' `majRev_factor` \tab Major revision multiplier of **incurred loss**
#' \[`claim_majRev_size()`\]. \cr
#' `majRev_atP` \tab An indicator, `1` if the last major revision occurs at the
#' time of the last major payment (i.e. second last payment), `0` otherwise
#' \[`claim_majRev_time()`\].
#' }
#' @seealso \code{\link[SynthETIC]{claims}}
#' @export
#' @name claim_majRev
claim_majRev_freq <- function(
  claims,
  rfun,
  paramfun,
  frequency_vector = claims$frequency_vector,
  claim_size_list = claims$claim_size_list,
  ...
) {

  if (!missing(rfun) & missing(paramfun)) {
    # we will see if we can continue without parameterisation
    # e.g. if the input rfun is directly a function of claim_size, and no
    # transformation is required
    paramfun <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default function to simulate the number of major revisions
  # NOTE: the default function takes as an assumption that there are max 3
  # major revisions -> if user wants to change this, they need to take care of
  # the later module (majRev_size) too
  if (missing(rfun)) {
    rfun <- function(
      # n = number of observations/claims
      n, claim_size, claim_size_benchmark) {

      if (n != length(claim_size)) {
        stop("The number of claims does not match.")
      }

      # inherit ref_claim from SynthETIC
      ref_claim <- SynthETIC::return_parameters()[1]

      # default benchmark value
      if (missing(claim_size_benchmark)) {
        claim_size_benchmark <- rep(0.075 * ref_claim, n)
      }

      k <- vector(length = n)
      for (i in 1:n) {
        if (claim_size[i] <= claim_size_benchmark[i]) {
          k[i] <- 1
        } else {
          Pr2 <- 0.1 + 0.3 *
            min(1, (claim_size[i] - 0.075*ref_claim)/(0.925*ref_claim))
          Pr3 <- 0.5 *
            min(1, max(0, claim_size[i] - 0.25*ref_claim)/(0.75*ref_claim))
          Pr1 <- 1 - Pr2 - Pr3
          k[i] <- sample(c(1, 2, 3), size = 1, replace = TRUE, prob = c(Pr1, Pr2, Pr3))
        }
      }

      k
    }

    # the default rfun directly takes claim_size as an input, so the "empty"
    # paramfun would do the trick
    paramfun <- function(...) {
      c(...)
    }
  }

  I <- length(frequency_vector)
  no_claims <- sum(frequency_vector)
  majRev <- vector("list", I)
  # majRev_unit stores all major revision information on a single claim
  majRev_unit <- list(
    majRev_freq = NA,
    majRev_time = NA,
    majRev_factor = NA,
    majRev_atP = NA
  )
  # set up the simulation parameters
  params <- mapply(paramfun,
                   claim_size = unlist(claim_size_list, use.names = FALSE),
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
  keep_formals <- c(list(n = no_claims), args[keep_names])

  if (paramfun_filled) {
    # check if the "empty" paramfun is sufficient to call the rfun
    tt <- try(no_majRev_vect <- do.call(rfun, keep_formals), TRUE)
    if (methods::is(tt, "try-error")) {
      stop("need to specify 'paramfun' for the sampling distribution")
    }
  } else {
    no_majRev_vect <- do.call(rfun, keep_formals)
  }

  curr <- 1 # curr tracks the claim number
  for (i in 1:I) {
    majRev[[i]] <- vector("list", frequency_vector[i])
    for (j in 1:frequency_vector[i]) {
      majRev[[i]][[j]] <- majRev_unit
      majRev[[i]][[j]]$majRev_freq <- no_majRev_vect[curr]
      curr <- curr + 1
    }
  }

  majRev
}


#' @rdname claim_majRev
#' @param claims an \code{\link[SynthETIC]{claims}} object containing all the
#' simulated quantities (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param majRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param rfun optional alternative random sampling function for:
#' * `claim_majRev_freq`: the number of major revisions;
#' * `claim_majRev_time`: the epochs of major revisions measured from claim
#' notification;
#' * `claim_majRev_size`: the sizes of the major revision multipliers.
#'
#' See Details for default.
#' @param paramfun parameters for the random sampling function, as a function of
#' other claim characteristics such as \code{claim_size}; see Details.
#' @param claim_size_list list of claim sizes (not required if the `claims`
#' argument is provided); see \code{\link[SynthETIC]{claim_size}}.
#' @param settlement_list list of settlement delays (not required if the
#' `claims` argument is provided); see \code{\link[SynthETIC]{claim_closure}}.
#' @param payment_delay_list (compound) list of inter partial delays (not
#' required if the `claims` argument is provided); see
#' \code{\link[SynthETIC]{claim_payment_delay}}.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @section Details - `claim_majRev_time` (Time): Let \eqn{\tau_k} represent the
#' epoch of the *k*th major revision (time measured from claim notification),
#' \eqn{k = 1, ..., K}. As the notification of a claim is considered a major
#' revision itself, we have \eqn{\tau_1 = 0} for all claims.
#'
#' The last major revision for a claim may occur at the time of the second last
#' partial payment (which is usually the major settlement payment) with
#' probability
#' \deqn{0.2 min(1, max(0, (claim_size - ref_claim) / (14 * ref_claim)))}
#' where `ref_claim` is a package-wise global variable that user should define
#' by \code{\link[SynthETIC]{set_parameters}} (if moving away from the default).
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
#' partial payment, `majRev_atP` (one of the output list components) will be set
#' to be 1.
#' @export
claim_majRev_time <- function(
  claims,
  majRev_list,
  rfun,
  paramfun,
  claim_size_list = claims$claim_size_list,
  settlement_list = claims$settlement_list,
  payment_delay_list = claims$payment_delay_list,
  ...
) {

  if (!missing(rfun) & missing(paramfun)) {
    # we will see if we can continue without parameterisation
    # e.g. if the input rfun is directly a function of claim_size, and no
    # transformation is required
    paramfun <- function(payment_delays, ...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default function to simulate the timing of major revisions (from notification)
  if (missing(rfun)) {
    rfun <- function(n, claim_size, setldel, penultimate_delay) {
      # n = number of simulations, here n should be the number of major revisions
      # penultimate_delay = delay from notification to penultimate payment
      majRev_time <- rep(NA, times = n)
      majRev_time[1] <- 0 # first revision at notification

      # inherit ref_claim from SynthETIC
      ref_claim <- SynthETIC::return_parameters()[1]

      if (n > 1) {
        # if the claim has multiple major revisions
        # does the last revision occur exactly at the second last partial payment?
        p <- 0.2 *
          min(1, max(0, (claim_size - ref_claim) / (14 * ref_claim)))
        at_second_last_pmt <- sample(c(0, 1), size = 1, replace = TRUE, prob = c(1-p, p))
        if (at_second_last_pmt == 0) {
          # no revision at second last payment
          majRev_time[2:n] <- sort(rtri(n - 1, min = setldel/3, max = setldel, mode = setldel/3))
        } else {
          # revision at second last payment
          majRev_time[n] <- penultimate_delay
          if (n > 2) {
            majRev_time[2:(n-1)] <- sort(
              rtri(n - 2, min = majRev_time[n]/3, max = majRev_time[n], mode = majRev_time[n]/3))
          }
        }
      }
      majRev_time
    }

    # the paramfun needs to account for the "computation" of penultimate_delay
    # from the payment_delay_list
    paramfun <- function(payment_delays, ...) {
      c(penultimate_delay = sum(payment_delays[1:length(payment_delays)-1]),
        ...)
    }
  }

  I <- length(majRev_list)
  params <- mapply(
    paramfun,
    claim_size = unlist(claim_size_list, use.names = FALSE),
    setldel = unlist(settlement_list, use.names = FALSE),
    # reduce payment_delays by one level only (to claim level)
    payment_delays = unlist(payment_delay_list, use.names = F, recursive = F),
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
    for (j in 1 : length(majRev_list[[i]])) {

      k <- majRev_list[[i]][[j]]$majRev_freq

      if (paramfun_filled) {
        tt <- try(majRev_list[[i]][[j]]$majRev_time <- do.call(
          rfun, c(as.list(args_df[, curr]), n = k)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun' for the sampling distribution")
        }
      } else {
        majRev_list[[i]][[j]]$majRev_time <- do.call(
          rfun, c(as.list(args_df[, curr]), n = k))
      }

      # is there a revision at second last payment?
      payment_delays <- payment_delay_list[[i]][[j]]
      no_pmt <- length(payment_delays)
      majRev_list[[i]][[j]]$majRev_atP <- ifelse(
        majRev_list[[i]][[j]]$majRev_time[k] == sum(payment_delays[1:(no_pmt - 1)]),
        1, 0)

      curr <- curr + 1
    }
  }

  majRev_list
}


#' @rdname claim_majRev
#' @param majRev_list nested list of major revision histories (with non-empty
#' revision frequencies).
#' @param rfun optional alternative random sampling function for:
#' * `claim_majRev_freq`: the number of major revisions;
#' * `claim_majRev_time`: the epochs of major revisions measured from claim
#' notification;
#' * `claim_majRev_size`: the sizes of the major revision multipliers.
#'
#' See Details for default.
#' @param paramfun parameters for the random sampling function, as a function of
#' other claim characteristics such as \code{claim_size}; see Details.
#' @param ... other arguments/parameters to be passed onto \code{paramfun}.
#'
#' @section Details - `claim_majRev_size` (Revision Multiplier): As mentioned in
#' the frequency section ("Details - `claim_majRev_freq`"), the default function
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
#' **The major revision multipliers apply to the incurred loss estimates**, that
#' is, a revision multiplier of 2.54 means that at the time of the major
#' revision the incurred loss increases by a factor of 2.54. We highlight this
#' as **in the case of minor revisions, the multipliers will instead apply to
#' outstanding claim amounts**, see \code{\link{claim_minRev}}.
#'
#' @examples
#' set.seed(1)
#' test_claims <- SynthETIC::test_claims_object
#' major <- claim_majRev_freq(test_claims)
#' major[[1]][[1]] # the "unit list" for the first claim
#'
#' # update the timing information
#' major <- claim_majRev_time(test_claims, major)
#' # observe how this has changed
#' major[[1]][[1]]
#'
#' # update the revision multipliers
#' major <- claim_majRev_size(major)
#' # again observe how this has changed
#' major[[1]][[1]]
#' @export
claim_majRev_size <- function(
  majRev_list,
  rfun,
  paramfun,
  ...
) {

  if (!missing(rfun) & missing(paramfun)) {
    # we will see if we can continue without parameterisation
    # e.g. if the input rfun is directly a function of claim_size, and no
    # transformation is required
    paramfun <- function(...) {
      c(...)
    }
    # paramfun_filled indicates whether an "empty" paramfun is taken by default
    paramfun_filled <- TRUE
  } else {
    paramfun_filled <- FALSE
  }

  # default function to simulate multiplier sizes
  # NOTE: this only works for up to 3 major revisions -> if user has adjusted
  # the majRev_freq_function, they will need to adjust this accordingly
  if (missing(rfun)) {
    rfun <- function(n) {
      # n = number of simulations, here n should be the number of major revisions
      majRev_factor <- rep(NA, times = n)
      majRev_factor[1] <- 1 # for first revision (at notification)
      if (n > 1) { # if the claim has multiple major revisions
        majRev_factor[2] <- stats::rlnorm(n = 1, meanlog = 1.8, sdlog = 0.2)
        if (n > 2) {
          mu <- 1 + 0.07 * (6 - majRev_factor[2])
          majRev_factor[3] <- stats::rlnorm(n = 1, meanlog = mu, sdlog = 0.1)
        }
      }

      majRev_factor
    }

    # the default rfun does not depend on other claim characteristics, so the
    # "empty" paramfun would do the trick
    paramfun <- function(...) {
      c(...)
    }

  }

  I <- length(majRev_list)
  params <- mapply(paramfun, ...)

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
    for (j in 1 : length(majRev_list[[i]])) {

      k <- majRev_list[[i]][[j]]$majRev_freq

      if (paramfun_filled) {
        tt <- try(majRev_list[[i]][[j]]$majRev_factor <- do.call(
          rfun, c(as.list(args_df[, curr]), n = k)))
        if (methods::is(tt, "try-error")) {
          stop("need to specify 'paramfun' for the sampling distribution")
        }
      } else {
        majRev_list[[i]][[j]]$majRev_factor <- do.call(
          rfun, c(as.list(args_df[, curr]), n = k))
      }

      curr <- curr + 1
    }
  }

  majRev_list

}
