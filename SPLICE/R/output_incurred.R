###############################################################################
##                             Output - Incurred                             ##
###############################################################################

#' Generate Incurred Dataset
#'
#' Generates a dataset of transaction records that tracks how the case estimates
#' of the total incurred, the outstanding claim payments, and the cumulative
#' claims paid change over time. A sample dataset is included as part of the
#' package, see \code{\link{test_incurred_dataset}}.
#'
#' @param claims an `claims` object containing all the simulated quantities,
#' (other than those related to incurred loss), see
#' \code{\link[SynthETIC]{claims}}.
#' @param incurred_history the full history of incurred case estimates, see
#' \code{\link{claim_history}}.
#'
#' @return A dataframe that takes the same structure as
#' \code{\link{test_incurred_dataset}}.
#' @seealso \code{\link{test_incurred_dataset}}
#' @export
generate_incurred_dataset <- function(
  claims, incurred_history) {

  I <- length(claims$frequency_vector)
  no_txn <- lengths(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_delay"))

  incurred_dataset <- data.frame(
    claim_no = rep(1:sum(claims$frequency_vector), times = no_txn),
    claim_size = rep(unlist(claims$claim_size_list), times = no_txn),
    txn_time = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_time")),
    txn_delay = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_delay")),
    txn_type = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_type")),
    incurred = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "incurred_right")),
    OCL = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "OCL_right")),
    cumpaid = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "cumpaid_right"))
  )

  # Extract and append the multiplier information
  majRev_factor <- minRev_factor_atP <- minRev_factor_notatP <- vector()
  tcount <- 0
  for (i in 1:I) {
    for (j in 1:claims$frequency_vector[i]) {
      extract_ma <- incurred_history[[i]][[j]]$majRev$majRev_factor
      extract_miP <- incurred_history[[i]][[j]]$minRev$minRev_factor_atP
      extract_miNP <- incurred_history[[i]][[j]]$minRev$minRev_factor_notatP
      num <- incurred_history[[i]][[j]]$minRev$minRev_freq_atP

      majRev_factor <- c(majRev_factor, extract_ma)
      minRev_factor_atP <- c(minRev_factor_atP, extract_miP)
      minRev_factor_notatP <- c(minRev_factor_notatP, extract_miNP)
      tcount <- tcount + num
    }
  }

  incurred_dataset[incurred_dataset$txn_type %in% c("Ma", "PMa"), "multiplier"] <-
    majRev_factor
  incurred_dataset[incurred_dataset$txn_type == "PMi", "multiplier"] <-
    minRev_factor_atP
  incurred_dataset[incurred_dataset$txn_type == "Mi", "multiplier"] <-
    minRev_factor_notatP

  incurred_dataset
}


#' Incurred Triangles
#'
#' Outputs the full square of claims incurred by occurrence period and
#' development period. The upper left triangle represents the past, and the
#' lower right triangle the unseen future. \cr \cr
#' Users can modify the aggregate level by providing an `aggregate_level`
#' argument to the function. For example, setting `aggregate_level = 4` when
#' working with calendar *quarters* produces an incurred square by occurrence
#' and development *year*. \cr \cr
#' We refer to the package vignette for examples on changing the aggregation
#' granularity:
#' \code{vignette("SPLICE-demo", package = "SPLICE")}
#'
#' @param incurred_history the full history of incurred case estimates, see
#' \code{\link{claim_history}}.
#' @param aggregate_level number of periods to be aggregated together; must be
#' a divisor of the total number of periods under consideration (default 1).
#' @param incremental logical; if true returns the incremental incurred square,
#' else returns the cumulative incurred square.
#' @param future logical; if true (default) shows the full claim triangle (i.e.
#' including claim payments in future periods), else shows only the past
#' triangle.
#' @return An array of claims incurred to date.
#' @details
#' **Remark on out-of-bound transaction times**: This function includes
#' adjustment for out-of-bound transaction dates, by forcing any transactions
#' that were projected to fall out of the maximum development period to be
#' counted as if they were made at the end of the limiting development period.
#' For example, if we consider 40 periods of development and a claim of the 21st
#' occurrence period was projected to have a major revision at time 62.498210,
#' then we would treat such a revision as if it occurred at time 60 for the
#' purpose of tabulation.
#' @export
output_incurred <- function(
  incurred_history,
  aggregate_level = 1,
  incremental = TRUE,
  future = TRUE) {

  frequency_vector <-  lengths(incurred_history)
  I <- length(frequency_vector)
  side <- I / aggregate_level
  incurred_cumulative <- array(0, c(side, side))
  colnames(incurred_cumulative) <- paste0("DP", 1:side)
  rownames(incurred_cumulative) <- paste0("AP", 1:side)
  adjustment <- 0 # track the number of corrections required for keeping all the
                  # transaction times within the bound

  for (i in 1:I) {
    i_rescaled <- ceiling(i / aggregate_level)

    for (j in 1:frequency_vector[i]) {

      # convert to discrete time scale (t in terms of absolute time)
      t <- ceiling(incurred_history[[i]][[j]]$txn_time / aggregate_level)
      incurred_right <- incurred_history[[i]][[j]]$incurred_right

      # Firstly need to treat the out-of-bound transaction times
      if (any(t - i_rescaled + 1 > side)) {
        t[which(t - i_rescaled + 1 > side)] <- i_rescaled + side - 1
        adjustment <- adjustment + 1
      }

      # Now get the latest incurred estimate in a period
      incurred_latest <- rep(NA, i_rescaled + side - 1)
      # Fill the incurred estimates
      incurred_latest[unique(t)] <- incurred_right[!rev(duplicated(rev(t)))]
      # Fill the NAs prior to the first non-NA with 0
      # (i.e. assume no incurred until claim notified)
      firstNonNA <- min(which(!is.na(incurred_latest)))
      if (firstNonNA > i_rescaled) {
        incurred_latest[i_rescaled:(firstNonNA - 1)] <- 0
      }
      # Fill the rest of NAs with the prior non-NAs
      incurred_latest <- zoo::na.locf(incurred_latest, na.rm = TRUE)
      for (k in 1:side) {
        incurred_cumulative[i_rescaled, k] <-
          incurred_cumulative[i_rescaled, k] + incurred_latest[k]
      }
    }
  }

  no_txn <- lengths(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_delay"))
  total_no_txn <- sum(no_txn)
  if (adjustment / total_no_txn > 0.03) {
    warning("More than 3% of the transactions were outside the bound.
    Check your data generation assumptions!")
  }

  if (incremental == TRUE) {
    incurred_incremental <- incurred_cumulative
    for (i in 1:dim(incurred_cumulative)[1]) {
      incurred_incremental[i, 1] <- incurred_cumulative[i, 1]
      for (j in 2:dim(incurred_cumulative)[2]) {
        incurred_incremental[i, j] <- incurred_cumulative[i, j] - incurred_cumulative[i, j - 1]
      }
    }

    if (future == TRUE) {
      incurred_incremental
    } else {
      # only to show the past triangle
      indicator <- apply(upper.tri(incurred_incremental[, 1:side], diag = TRUE), 1, rev)
      incurred_incremental[!indicator] <- NA

      incurred_incremental
    }

  } else {
    if (future == TRUE) {
      incurred_cumulative
    } else {
      # only to show the past triangle
      indicator <- apply(upper.tri(incurred_cumulative[, 1:side], diag = TRUE), 1, rev)
      incurred_cumulative[!indicator] <- NA

      incurred_cumulative
    }
  }

}
