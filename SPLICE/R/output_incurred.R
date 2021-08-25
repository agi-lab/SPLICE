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
#' see \code{\link{claims}}.
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
  no_txn <- lengths(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_time"))

  incurred_dataset <- data.frame(
    claim_no = rep(1:sum(claims$frequency_vector), times = no_txn),
    claim_size = rep(unlist(claims$claim_size_list), times = no_txn),
    txn_time = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_t")),
    txn_dalay = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_time")),
    txn_type = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_type")),
    incurred = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "incurred_right")),
    OCL = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "OCL_right")),
    paid = unlist(lapply(unlist(incurred_history, recursive = F), `[[`, "paid_right"))
  )

  # Extract and append the multiplier information
  maRev_multiplier <- miRev_multiplier_atP <- miRev_multiplier_NatP <- vector()
  tcount <- 0
  for (i in 1:I) {
    for (j in 1:claims$frequency_vector[i]) {
      extract_ma <- incurred_history[[i]][[j]]$maRev$maRev_multiplier
      extract_miP <- incurred_history[[i]][[j]]$miRev$miRev_multiplier_atP
      extract_miNP <- incurred_history[[i]][[j]]$miRev$miRev_multiplier_NatP
      num <- incurred_history[[i]][[j]]$miRev$miRev_no_atP

      maRev_multiplier <- c(maRev_multiplier, extract_ma)
      miRev_multiplier_atP <- c(miRev_multiplier_atP, extract_miP)
      miRev_multiplier_NatP <- c(miRev_multiplier_NatP, extract_miNP)
      tcount <- tcount + num
    }
  }

  incurred_dataset[incurred_dataset$txn_type %in% c("Ma", "PMa"), "multiplier"] <-
    maRev_multiplier
  incurred_dataset[incurred_dataset$txn_type == "PMi", "multiplier"] <-
    miRev_multiplier_atP
  incurred_dataset[incurred_dataset$txn_type == "Mi", "multiplier"] <-
    miRev_multiplier_NatP

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
#' and development *year*.
#'
#' @param incurred_history the full history of incurred case estimates, see
#' \code{\link{claim_history}}.
#' @param aggregate_level number of periods to be aggregated together; must be
#' a divisor of the total number of periods under consideration (default 1).
#' @param incremental logical; if true returns the incremental incurred square,
#' else returns the cumulative incurred square.
#' @return An array of claims incurred to date.
#' @details
#' **Remark on out-of-bound transaction times**: This function includes
#' adjustment for out-of-bound transaction dates, by forcing any transactions
#' made beyond the maximum development period to be counted as if at the end
#' of the limiting development period. For example, if we consider 40
#' periods of development and a claim of the 21st occurrence period was
#' projected to have a major revision at time 62.498210, then we would treat
#' such a revision as if it occurred at time 60 for the purpose of tabulation.
#' @export
incurred_output <- function(
  incurred_history,
  aggregate_level = 1,
  incremental = TRUE) {

  frequency_vector <-  lengths(incurred_history)
  I <- length(frequency_vector)
  incurred_cumulative <- array(0, c(I, I))
  adjustment <- 0 # track the number of corrections required for keeping all the
                  # transaction times within the bound

  for (i in 1:I) {
    for (j in 1:frequency_vector[i]) {
      # convert to discrete time scale (t in terms of absolute time)
      t <- ceiling(incurred_history[[i]][[j]]$txn_t)
      incurred_right <- incurred_history[[i]][[j]]$incurred_right

      # Firstly need to treat the out-of-bound transaction times
      if (any(t - i + 1 > I)) {
        t[which(t - i + 1 > I)] <- i + I - 1
        adjustment <- adjustment + 1
      }
      # Now get the latest incurred estimate in a period
      incurred_latest <- rep(NA, i + I - 1)
      # Fill the incurred estimates
      incurred_latest[unique(t)] <- incurred_right[!rev(duplicated(rev(t)))]
      # Fill the NAs prior to the first non-NA with 0
      # (i.e. assume no incurred until claim notified)
      firstNonNA <- min(which(!is.na(incurred_latest)))
      if (firstNonNA > i) {
        incurred_latest[i:(firstNonNA - 1)] <- 0
      }
      # Fill the rest of NAs with the prior non-NAs
      incurred_latest <- zoo::na.locf(incurred_latest, na.rm = TRUE)
      for (k in 1:I) {
        incurred_cumulative[i, k] <- incurred_cumulative[i, k] + incurred_latest[k]
      }
    }
  }

  no_txn <- lengths(lapply(unlist(incurred_history, recursive = F), `[[`, "txn_time"))
  total_no_txn <- sum(no_txn)
  if (adjustment / total_no_txn > 0.03) {
    warning("More than 3% of the transactions were outside the bound.")
  }

  if (aggregate_level != 1) {
    # if aggregate at a higher level (e.g. aggregate_level = 4 for yearly triangles)
    new_side_length <- I / aggregate_level
    incurred_cumulative_orig <- incurred_cumulative
    incurred_cumulative <- array(0, c(new_side_length, new_side_length))

    for (i in 1:new_side_length) {
      side_occurrence <- (aggregate_level * (i-1) + 1): (aggregate_level * i)
      for (j in 1:new_side_length) {
        side_development <- aggregate_level * j
        incurred_cumulative[i, j] <- sum(
          incurred_cumulative_orig[side_occurrence, side_development])
      }
    }
  }

  if (incremental == TRUE) {
    incurred_incremental <- incurred_cumulative
    for (i in 1:dim(incurred_cumulative)[1]) {
      incurred_incremental[i, 1] <- incurred_cumulative[i, 1]
      for (j in 2:dim(incurred_cumulative)[2]) {
        incurred_incremental[i, j] <- incurred_cumulative[i, j] - incurred_cumulative[i, j - 1]
      }
    }
    incurred_incremental
  } else {
    incurred_cumulative
  }
}
