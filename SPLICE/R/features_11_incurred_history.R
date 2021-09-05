###############################################################################
##                      11. Development of case estimates                    ##
###############################################################################

# Development of case estimates
# Helper function (hidden from users)
individual_claim_history <- function(
  majRev, minRev,
  claim_size, no_pmt, payment_delays, payment_sizes, occurrence, notidel,
  k1, k2, # k1_inv for major revision, k2_inv for minor revision
  inflated, base_inflation_vector,
  keep_all) {

  # convert the supplied inflation rates to a continuous index function
  # note that q is in quarters, so when we apply this function we need to convert time to quarters
  base_inflation <- function(q) {
    if (q == 0) {
      return(1)
    }
    # rate to index
    # e.g. (0.03, 0.01, 0.02) is converted to (1.03, 1.03*1.01, 1.03*1.01*1.02)
    base_inflation_index <- cumprod(1 + base_inflation_vector)

    # assume continuous compounding within quarters
    # e.g. if q = 1.82, returns 1.03 * 1.01^(0.82)
    # cap q = max quarter, ie set inflation to that of the end of max period
    if (q >= length(base_inflation_vector)) {
      q <- length(base_inflation_vector)
    }
    max(base_inflation_index[floor(q)], 1) * (1 + base_inflation_vector[ceiling(q)])^(q - floor(q))
  }
  base_inflation <- Vectorize(base_inflation)

  # rewrite the inter-partial delays as delays from notification
  Ptimes <- cumsum(payment_delays)
  # incurred revision indicator
  rev_atP <- minRev$minRev_atP
  rev_atP <- ifelse(rev_atP == 1, "PMi", "P")
  if (majRev$majRev_atP == 1) {
    rev_atP[no_pmt - 1] <- "PMa"
    # if there is a simultaneous majRev and minRev at the second last payment
    # then discard the minor revision
    if (minRev$minRev_atP[no_pmt - 1] == 1) {
      no_minRev <- minRev$minRev_freq_atP
      if (minRev$minRev_atP[no_pmt] == 0) {
        # no minor revision at the last payment
        minRev$minRev_time_atP <- minRev$minRev_time_atP[-no_minRev]
        minRev$minRev_factor_atP <- minRev$minRev_factor_atP[-no_minRev]
      } else {
        # if there was a minor revision at the last payment too
        minRev$minRev_time_atP <- minRev$minRev_time_atP[-(no_minRev - 1)]
        minRev$minRev_factor_atP <- minRev$minRev_factor_atP[-(no_minRev - 1)]
      }
      minRev$minRev_atP[no_pmt - 1] <- 0
      minRev$minRev_freq_atP <- minRev$minRev_freq_atP - 1
    }
  }
  # insert the revisions that do not occur at a payment
  majRev_freqtatP <- majRev$majRev_time
  if (majRev$majRev_atP == 1) {
    majRev_freqtatP <- majRev_freqtatP[-majRev$majRev_freq]
  }
  minRev_freqtatP <- minRev$minRev_time_notatP
  txn_delay <- sort(c(Ptimes, majRev_freqtatP, minRev_freqtatP))
  txn_time <- txn_delay + occurrence + notidel # in absolute time
  txn_type <- rep(NA, length(txn_delay))
  txn_type[txn_delay %in% Ptimes] <- rev_atP
  txn_type[txn_delay %in% majRev_freqtatP] <- "Ma"
  txn_type[txn_delay %in% minRev_freqtatP] <- "Mi"
  stopifnot(txn_type %in% c("P", "PMi", "PMa", "Mi", "Ma"))

  # need cumulative claims paid, outstanding claims liability and incurred
  # at time of each transaction
  # _left denotes right before the transaction (t - 0) and _right denotes after
  no_txn <- length(txn_delay)
  c_left <- x_left <- y_left <- c_right <- x_right <- y_right <- rep(NA, no_txn)
  p_index <- no_pmt
  Ma_index <- majRev$majRev_freq
  Mi_index_atP <- minRev$minRev_freq_atP
  Mi_index_notatP <- minRev$minRev_freq_notatP
  Ma_multp <- majRev$majRev_factor
  Mi_multp_atP <- minRev$minRev_factor_atP
  Mi_multp_notatP <- minRev$minRev_factor_notatP

  # inherit time unit from SynthETIC
  time_unit <- SynthETIC::return_parameters()[2]

  for (i in no_txn:2) {
    if (i == no_txn) {
      # initialise at claim closure
      if (inflated == FALSE) {
        c_right[no_txn] <- claim_size
        x_right[no_txn] <- 0
        y_right[no_txn] <- claim_size
      } else {
        # for inflation == TRUE, get the inflated incurred at settlement
        c_right[no_txn] <- sum(payment_sizes)
        x_right[no_txn] <- 0
        y_right[no_txn] <- sum(payment_sizes)
      }
    }

    # incurred revision, in reverse chronological order
    if (startsWith(txn_type[i], "P")) {
      c_right[i - 1] <- c_left[i] <- c_right[i] - payment_sizes[p_index]
      if (txn_type[i] == "PMa") {
        # adjustment for base inflation
        # need the time of the "next" revision
        sset <- txn_time[which(txn_type[1:(i - 1)] %in% c("Ma", "Mi", "PMa", "PMi"))]
        next_rev_time <- max(sset[length(sset)], 0)
        discount <-
          base_inflation(next_rev_time * time_unit * 4) /
          base_inflation(txn_time[i] * time_unit * 4)
        y_left[i] <- y_right[i] * discount

        # need y >= k1_inv * c after the retrospective revision
        k1_inv <- 1 / k1
        y_left[i] <- y_left[i] / Ma_multp[Ma_index]
        y_right[i - 1] <- y_left[i] <- max(y_left[i], k1_inv * c_left[i])
        x_right[i - 1] <- x_left[i] <- y_left[i] - c_left[i]
        Ma_index <- Ma_index - 1
      } else if (txn_type[i] == "PMi") {
        # adjustment for base inflation
        # need the time of the "next" revision
        sset <- txn_time[which(txn_type[1:(i - 1)] %in% c("Ma", "Mi", "PMa", "PMi"))]
        next_rev_time <- max(sset[length(sset)], 0)
        discount <-
          base_inflation(next_rev_time * time_unit * 4) /
          base_inflation(txn_time[i] * time_unit * 4)
        y_left[i] <- y_right[i] * discount

        # need y >= k2_inv * c after the retrospective revision
        k2_inv <- 1 / k2
        x_left[i] <- (y_left[i] - c_left[i]) / Mi_multp_atP[Mi_index_atP]
        y_left[i] <- x_left[i] + c_left[i]

        y_right[i - 1] <- y_left[i] <- max(y_left[i], k2_inv * c_left[i])
        x_right[i - 1] <- x_left[i] <- y_left[i] - c_left[i]
        Mi_index_atP <- Mi_index_atP - 1
      } else {
        # payment without incurred revision
        y_right[i - 1] <- y_left[i] <- y_right[i]
        x_right[i - 1] <- x_left[i] <- y_left[i] - c_left[i]
      }
      p_index <- p_index - 1
    } else {
      c_right[i - 1] <- c_left[i] <- c_right[i]
      if (txn_type[i] == "Ma") {
        # adjustment for base inflation
        # need the time of the "next" revision
        sset <- txn_time[which(txn_type[1:(i - 1)] %in% c("Ma", "Mi", "PMa", "PMi"))]
        next_rev_time <- max(sset[length(sset)], 0)
        discount <-
          base_inflation(next_rev_time * time_unit * 4) /
          base_inflation(txn_time[i] * time_unit * 4)
        y_left[i] <- y_right[i] * discount

        # need y >= k1_inv * c after the retrospective revision
        k1_inv <- 1 / k1
        y_left[i] <- y_left[i] / Ma_multp[Ma_index]
        y_right[i - 1] <- y_left[i] <- max(y_left[i], k1_inv * c_left[i])
        x_right[i - 1] <- x_left[i] <- y_left[i] - c_left[i]
        Ma_index <- Ma_index - 1
      } else {
        # adjustment for base inflation
        # need the time of the "next" revision
        sset <- txn_time[which(txn_type[1:(i - 1)] %in% c("Ma", "Mi", "PMa", "PMi"))]
        next_rev_time <- max(sset[length(sset)], 0)
        discount <-
          base_inflation(next_rev_time * time_unit * 4) /
          base_inflation(txn_time[i] * time_unit * 4)
        y_left[i] <- y_right[i] * discount

        # need y >= k2_inv * c after the retrospective revision
        k2_inv <- 1 / k2
        x_left[i] <- (y_left[i] - c_left[i]) / Mi_multp_notatP[Mi_index_notatP]
        y_left[i] <- x_left[i] + c_left[i]

        y_right[i - 1] <- y_left[i] <- max(y_left[i], k2_inv * c_left[i])
        x_right[i - 1] <- x_left[i] <- y_left[i] - c_left[i]
        Mi_index_notatP <- Mi_index_notatP - 1
      }
    }
  }

  stopifnot(Ma_index == 1 && Mi_index_atP == 0 && Mi_index_notatP == 0 && p_index == 0)
  y_left[1] <- y_right[1]
  x_left[1] <- x_right[1]
  c_left[1] <- c_right[1] <- 0 # claim paid at notification should be zero

  if (keep_all == TRUE) {
    result <- list(
      txn_delay = txn_delay, txn_time = txn_time, txn_type = txn_type,
      cumpaid_left = c_left, cumpaid_right = c_right,
      OCL_left = x_left, OCL_right = x_right,
      incurred_left = y_left, incurred_right = y_right,
      minRev = minRev, majRev = majRev)
  } else {
    result <- list(
      txn_delay = txn_delay,
      txn_time = txn_time,
      txn_type = txn_type,
      cumpaid_right = c_right,
      OCL_right = x_right,
      incurred_right = y_right,
      minRev = minRev,
      majRev = majRev)
  }

  return(result)
}


#' Development of Case Estimates
#'
#' Consolidates payments and incurred revisions and returns a full transactional
#' history of all the individual claims (transaction being either a payment or
#' a case estimate revision).
#'
#' @param claims an `claims` object containing all the simulated quantities
#' (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param majRev_list nested list of major revision histories, see
#' \code{\link{claim_majRev}}.
#' @param minRev_list nested list of minor revision histories, see
#' \code{\link{claim_minRev}}.
#' @param k1 maximum amount of cumulative claims paid as a proportion of
#' total incurred estimate for major revisions; between 0 and 1.
#' @param k2 maximum amount of cumulative claims paid as a proportion of
#' total incurred estimate for minor revisions; between 0 and 1.
#' @param base_inflation_vector vector showing **quarterly** base inflation
#' rates (quarterly effective) for all the periods under consideration (default
#' is nil base inflation), should be consistent with the input inflation vector
#' in \code{\link[SynthETIC]{claim_payment_inflation}}. If a single number is
#' provided, the function will assume constant quarterly inflation.
#' @param keep_all ``TRUE`` to keep the paid, outstanding payments, total
#' incurred estimates just before the revision, ``FALSE`` to keep only the
#' estimates right after the revision (`_right`).
#'
#' @details This function works to generate the full history of claims paid and
#' incurred estimates by consolidating all the simulated revision quantities.
#' It should be noted that in this consolidation step, we make the following
#' adjustments:
#' - Major and minor revisions should not occur simultaneously. In the event
#' that they do (which is only possible at the second last partial payment),
#' the major revision takes precedence, and the minor revision be discarded.
#' This will be reflected in the `majRev` and `minRev` components of the output
#' list.
#' - Estimates of incurred loss are specified to be computed in reverse order,
#' and it is necessary that the total incurred estimate is always strictly
#' greater than the cumulative claims paid (except at the final paymen where
#' equality holds). Hence we introduce `k1` and `k2` to make sure that
#' the revised incurred estimates satisfy \deqn{ky(t) \ge c(t)} where \eqn{y(t)}
#' represents the total incurred estimate at delay \eqn{t}, \eqn{k} is a
#' constant between 0 and 1, and \eqn{c(t)} is the cumulative claims paid up to
#' time \eqn{t}. When the raw simulated revision multipliers violate this
#' requirement, the case estimates of the total incurred or the outstanding
#' claim payments will be increased to make sure this condition always holds,
#' i.e. this adjustment takes precedence over the raw simulated revision
#' multipliers.
#' - **Inflation adjustment**: One can choose to ignore inflation in the
#' incurred estimates (default), or to make allowance for
#' it.
#'   - If `base_inflation_vector == NULL` (default), then all case estimates
#'   will be computed in values corresponding to time \eqn{t = 0}, i.e. the
#'   commencement of the first occurrence period.
#'   - If `base_inflation_vector` is provided, then the case estimators will
#'   include full superimposed inflation and base inflation only up to the date
#'   of the revision, i.e. there is an adjustment for the time elapsed since the
#'   immediately preceding revision and **no future base inflation** beyond the
#'   date of valuation.
#'   - If inflation is involved, it should be noted that the case estimator
#'   reviews the base inflation situation only in the process of making a
#'   revision. When *only* a payment is made, the insurer's system automatically
#'   writes down the outstanding liability on the assumption of no change in
#'   ultimate incurred amount.
#'
#' @return A nested list structure such that the *j*th component of the *i*th
#' sub-list provides a full transactional history of the *j*th claim of
#' occurrence period *i*. The "unit list" (i.e. the smallest, innermost
#' sub-list) contains the following components:
#' \tabular{ll}{
#' `txn_delay` \tab Delays from notification to the transactions (payment or
#' incurred revision). \cr
#' `txn_time` \tab Times of the transactions (from the commencement of the
#' first occurrence period). \cr
#' `txn_type` \tab Types of the transactions, "Ma" for major revision, "Mi" for
#' minor revision, "P" for payment, "PMa" for major revision coincident with a
#' payment, "PMi" for minor revision coincident with a payment. \cr
#' `cumpaid_right` \tab Cumulative claim payments immediately after each of the
#' transactions (in the "right" continuous sense). \cr
#' `OCL_right` \tab Case estimate of outstanding claim payments immediately
#' after each of the transactions (in the "right" continuous sense). \cr
#' `incurred_right` \tab Case estimate of incurred loss immediately after each
#' of the transactions (in the "right" continuous sense). \cr
#' `minRev` \tab A list containing full history of minor revisions (frequency,
#' time and revision size); \code{\link{claim_minRev}}. \cr
#' `majRev` \tab A list containing full history of major revisions (frequency,
#' time and revision size); see \code{\link{claim_majRev}}.
#' }
#'
#' and optionally (by setting `keep_all = TRUE`),
#' \tabular{ll}{
#' `cumpaid_left` \tab Cumulative claim payments just before each of the
#' transactions. \cr
#' `OCL_left` \tab Case estimate of outstanding claim payments just before each
#' of the transactions. \cr
#' `incurred_left` \tab Case estimate of incurred loss just before each of the
#' transactions. \cr
#' }
#' @export
claim_history <- function(
  claims,
  majRev_list,
  minRev_list,
  k1 = 0.95, k2 = 0.95, # k1 for major revision, k2 for minor revision
  base_inflation_vector = NULL,
  keep_all = FALSE
) {

  I <- length(claims$frequency_vector)
  # convert to number of calendar quarters (floor should be unnecessary)
  # times 2 to get the maximum calendar period
  time_unit <- SynthETIC::return_parameters()[2]
  max_quarters <- floor(I * time_unit * 4) * 2

  # set nil base inflation by default
  if (is.null(base_inflation_vector)) {
    base_inflation_vector <- rep(0, times = max_quarters)
    inflated <- FALSE
  } else if (length(base_inflation_vector) == 1) {
    base_inflation_vector <- rep(base_inflation_vector, times = max_quarters)
    inflated <- TRUE
  } else if (length(base_inflation_vector) != max_quarters) {
    stop("base_inflation_vector is of a wrong size. Either input a single value
    or a vector of appropriate length")
  } else {
    inflated <- TRUE
  }

  full_history <- vector("list", I)
  for (i in 1:I) {
    full_history[[i]] <- vector("list", claims$frequency_vector[i])
    for (j in 1:claims$frequency_vector[i]) {
      if (inflated == TRUE) {
        payment_sizes <- claims$payment_inflated_list[[i]][[j]]
      } else {
        payment_sizes <- claims$payment_size_list[[i]][[j]]
      }

      full_history[[i]][[j]] <- individual_claim_history(
        majRev = majRev_list[[i]][[j]],
        minRev = minRev_list[[i]][[j]],
        claim_size = claims$claim_size[[i]][j],
        no_pmt = claims$no_payments_list[[i]][j],
        payment_delays = claims$payment_delay_list[[i]][[j]],
        payment_sizes = payment_sizes,
        occurrence = claims$occurrence_list[[i]][j],
        notidel = claims$notification_list[[i]][j],
        k1 = k1,
        k2 = k2,
        inflated = inflated,
        base_inflation_vector = base_inflation_vector,
        keep_all = keep_all
      )
    }
  }

  full_history
}
