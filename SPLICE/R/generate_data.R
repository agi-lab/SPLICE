###############################################################################
##                      Generation of Alternative Data                       ##
###############################################################################

# Get pre-specified parameters for a given level of complexity
# Helper function (hidden from users)
get_params <- function(complexity) {

  complexity = as.character(complexity)

  if (complexity == '1') {
    # Simple, CL compatible dataset
    # Remove the dependency of notification delay on occurrence period and claim size
    ntfy_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- 1
      target_cv <- 0.70
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Remove the dependency of claim closure on occurrence period and claim size
    closure_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(25, max(1, 6 + 4 * log(claim_size / 20000)))
      target_cv <- 0.30
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Turn off base inflation
    base_rate <- 0

    # Turn off occurrence period superimposed inflation
    si_occurrence_function <- function(occurrence_time, claim_size) {
      return(1)
    }

    # Turn off calendar period superimposed inflation
    si_payment_funtion <- function(payment_time, claim_size) {
      return(1)
    }

  } else if (complexity == '2') {

    # Slightly more complex, but still CL compatible
    # Let notification closure delays depend on claim size, but not occurrence
    # period to preserve CL compatibility
    ntfy_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(3, max(1, 2 - log(claim_size/100000) / 3))
      target_cv <- 0.70
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Remove the dependency of claim closure on occurrence period
    closure_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(25, max(1, 6 + 4 * log(claim_size / 20000)))
      target_cv <- 0.30
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Constant base inflation at 2%
    base_rate <- (1 + 0.02)^(1/4) - 1

    # Turn off occurrence period superimposed inflation
    si_occurrence_function <- function(occurrence_time, claim_size) {
      return(1)
    }

    # Turn off calendar period superimposed inflation
    si_payment_funtion <- function(payment_time, claim_size) {
      return(1)
    }

  } else if (complexity == '3') {

    # Allow closure delays to depend on occurrence period
    # Increase in claim processing speed
    # Keep notification delay function unchanged
    ntfy_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(3, max(1, 2 - log(claim_size/100000) / 3))
      target_cv <- 0.70
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    closure_paramfun <- function(claim_size, occurrence_period, ...) {
      a <- 1 - 0.0015 * occurrence_period
      target_mean <- a * min(25, max(1, 6 + 4 * log(claim_size / 20000)))
      target_cv <- 0.30
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Constant base inflation at 2%
    base_rate <- (1 + 0.02)^(1/4) - 1

    # Turn off occurrence period superimposed inflation
    si_occurrence_function <- function(occurrence_time, claim_size) {
      return(1)
    }

    # Turn off calendar period superimposed inflation
    si_payment_funtion <- function(payment_time, claim_size) {
      return(1)
    }

  } else if (complexity == '4') {

    # Inflation shock at AQ30 (from 0% to 10% p.a.)
    # Set notification and closure delays to be independent of AQ
    ntfy_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(3, max(1, 2 - log(claim_size/100000) / 3))
      target_cv <- 0.70
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Remove the dependency of claim closure on occurrence period
    closure_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(25, max(1, 6 + 4 * log(claim_size / 20000)))
      target_cv <- 0.30
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Constant base inflation at 2%
    base_rate <- (1 + 0.02)^(1/4) - 1

    # Turn off occurrence period superimposed inflation
    si_occurrence_function <- function(occurrence_time, claim_size) {
      return(1)
    }

    # Introduce inflation shock at time 30
    si_payment_funtion <- function(payment_time, claim_size) {
      if (payment_time <= 30) {
        return(1)
      } else {
        period_rate <- (1 + 0.10)^(1/4) - 1
        return((1 + period_rate)^(payment_time - 30))
      }
    }
    si_payment_funtion <- Vectorize(si_payment_funtion)

  } else if (complexity == '5') {
    # Default specification (i.e. most complex)
    ntfy_paramfun <- function(claim_size, occurrence_period, ...) {
      target_mean <- min(3, max(1, 2 - log(claim_size/100000) / 3))
      target_cv <- 0.70
      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    closure_paramfun <- function(claim_size, occurrence_period, ...) {
      if (claim_size < 20000 & occurrence_period >= 21) {
        a <- min(0.85, 0.65 + 0.02 * (occurrence_period - 21))
      } else {
        a <- max(0.85, 1 - 0.0075 * occurrence_period)
      }

      target_mean <- a * min(25, max(1, 6 + 4 * log(claim_size / 20000)))
      target_cv <- 0.60

      params <- get_Weibull_parameters(target_mean, target_cv)
      return(c(shape = params[1, ], scale = params[2, ]))
    }

    # Constant base inflation at 2%
    base_rate <- (1 + 0.02)^(1/4) - 1

    si_occurrence_function <- function(occurrence_time, claim_size) {
      if (occurrence_time <= 20) {1}
      else {1 - 0.4 * max(0, 1 - claim_size / 50000)}
    }

    si_payment_funtion <- function(payment_time, claim_size) {
      period_rate <- (1 + 0.30)^(1/4) - 1
      beta <- period_rate * max(0, 1 - claim_size / 200000)
      (1 + beta)^payment_time
    }
  }

  return(list(
    ntfy_paramfun = ntfy_paramfun,
    closure_paramfun = closure_paramfun,
    base_rate = base_rate,
    si_occurrence_function = si_occurrence_function,
    si_payment_funtion = si_payment_funtion
  ))
}

#' Generate Data of Varying Complexity
#'
#' `r lifecycle::badge("experimental")` \cr \cr
#' Generates datasets under 5 scenarios of different levels of complexity (here
#' "complexity" means the level of difficulty of analysis).
#'
#' @param n_claims_per_period **expected** number of claims per period (equals
#' the total expected number of claims divided by `n_periods`).
#' @param n_periods number of accident periods considered (equals number of
#' claims development periods considered); default 40.
#' @param complexity integer from 1 (simplest) to 5 (most complex); see Details.
#' @param data_type a character vector specifying output data types. By default
#' the function will output all 3 datasets (claims, payments, incurred), but the
#' user may choose to output only a subset.
#' @param random_seed optional seed for random number generation for
#' reproducibility.
#' @param verbose logical; if `TRUE` print a message about the data generated.
#' @param covariates_obj a SynthETIC \code{\link[SynthETIC]{covariates}} object (requires `SynthETIC >= 1.1.0`). Defaults to `NULL`.
#'
#' @details `generate_data()` produces datasets of varying levels of complexity,
#' where 1 represents the simplest, and 5 represents the most complex:
#' * 1 – simple, homogeneous claims experience, with zero inflation.
#' * 2 – slightly more complex than 1, with dependence of notification delay and
#' settlement delay on claim size, and 2% p.a. base inflation.
#' * 3 – steady increase in claim processing speed over occurrence periods (i.e.
#' steady decline in settlement delays).
#' * 4 – inflation shock at time 30 (from 0% to 10% p.a.).
#' * 5 – default distributional models, with complex dependence structures (e.g.
#' dependence of settlement delay on claim occurrence period).
#'
#' We remark that this by no means defines the limits of the complexity that can
#' be generated with `SPLICE`. This function is provided for the convenience of
#' users who wish to generate (a collection of) datasets under some
#' representative scenarios. If more complex features are required, the user is
#' free to modify the distributional assumptions (which, of course, requires
#' more thoughts and coding) to achieve their purposes.
#'
#' @return A named list of dataframes:
#' \tabular{ll}{
#' `claim_dataset` \tab A dataset of claim records that takes the same structure
#' as \code{\link[SynthETIC]{test_claim_dataset}}, with each row representing a
#' unique claim. \cr
#' `payment_dataset` \tab A dataset of partial payment records that takes the
#' same structure as \code{\link[SynthETIC]{test_transaction_dataset}}, with
#' each row representing a unique payment. \cr
#' `incurred_dataset` \tab A dataset of transaction records that tracks how the
#' case estimates change over time. Takes the same structure as
#' \code{\link{test_incurred_dataset}}, with each row representing a transaction
#' (any of claim notification, settlement, a payment, or a case estimate
#' revision). \cr
#' `covariates_data` \tab Only if `covariates_obj` is not NULL, in which case
#' it will return a SynthETIC \code{\link[SynthETIC]{covariates_data}} object.
#' }
#'
#' @seealso \code{\link[SynthETIC]{generate_claim_dataset}},
#' \code{\link[SynthETIC]{generate_transaction_dataset}},
#' \code{\link{generate_incurred_dataset}}
#'
#' @examples
#' # Generate datasets of full complexity
#' result <- generate_data(
#'   n_claims_per_period = 50, data_type = c('claims', 'payments'),
#'   complexity = 5, random_seed = 42)
#'
#' # Save individual datasets
#' claims <- result$claim_dataset
#' payments <- result$payment_dataset
#'
#' # Generate chain-ladder compatible dataset
#' CL_simple <- generate_data(
#'   n_claims_per_period = 50, data_type = 'claims', complexity = 1, random_seed = 42)
#'
#' # To mute message output
#' CL_simple_2 <- generate_data(
#'   n_claims_per_period = 50, data_type = 'claims', verbose = FALSE, random_seed = 42)
#'
#' # Ouput is reproducible with the same random_seed value
#' all.equal(CL_simple$claim_dataset, CL_simple_2$claim_dataset)
#'
#' @export
generate_data <- function(
  n_claims_per_period,
  n_periods = 40,
  complexity = c(1:5),
  data_type = c("claims", "payments", "incurred"),
  random_seed = NULL,
  verbose = TRUE,
  covariates_obj = NULL
) {

  # match.arg only works with chars
  complexity <- as.character(complexity)
  complexity <- match.arg(complexity, paste(1:5))
  data_type <- match.arg(data_type, several.ok = TRUE)
  if (!is.null(random_seed)) {
    set.seed(random_seed)
  }

  # Print a description of the scenario considered
  if (verbose) {
    descriptions = list(
      '1' = '(scenario: simple, chain-ladder compatible)',
      '2' = '(scenario: claim development depends on claim size)',
      '3' = '(scenario: increase in claim processing speed over accident periods)',
      '4' = '(scenario: inflation shock at time 30 (from 0% to 10% p.a.))',
      '5' = '(scenario: full complexity)')
    print(paste(
      "Generating", paste(data_type, collapse = ', '), "data with complexity =",
      complexity, descriptions[[complexity]], ",,,"))
  }

  params <- get_params(complexity)
  output <- list(
    claim_dataset = NULL,
    payment_dataset = NULL,
    incurred_dataset = NULL
  )

  # M1: Claim occurrence
  # Number of claims occurring for each period i
  freq_vector <- claim_frequency(
    I = n_periods, simfun = stats::rpois, lambda = n_claims_per_period)
  # Occurrence time of each claim r, for each period i
  occurrence_times <- claim_occurrence(freq_vector)

  # M2: Claim size (constant dollar values)
  claim_sizes <- claim_size(freq_vector)
  # M2a: Simulation of Covariates
  if (!is.null(covariates_obj) & isa(covariates_obj, "covariates")) {
    claim_size_covariates <- claim_size_adj(covariates_obj, claim_sizes)
    covariates_data_obj <- claim_size_covariates$covariates_data
    claim_sizes <- claim_size_covariates$claim_size_adj
    output$covariates_data <- covariates_data_obj
  }

  # M3: Claim notification
  ntfy_delays <- claim_notification(
    freq_vector, claim_sizes, paramfun = params$ntfy_paramfun)

  # M4: Claim closure
  # Remove the dependency of claim closure on occurrence period
  setl_delays <- claim_closure(
    freq_vector, claim_sizes, paramfun = params$closure_paramfun)

  # M5: Number of Partial payments
  no_payments <- claim_payment_no(freq_vector, claim_sizes)

  if (length(data_type) == 1 && data_type == 'claims') {
    output$claim_dataset <- generate_claim_dataset(
      frequency_vector = freq_vector,
      occurrence_list = occurrence_times,
      claim_size_list = claim_sizes,
      notification_list = ntfy_delays,
      settlement_list = setl_delays,
      no_payments_list = no_payments)

    # Exit early because we don't need to simulate the payment information
    return(output)
  }

  # M6: Sizes of Partial payments (constant dollar values)
  payment_sizes <- claim_payment_size(freq_vector, claim_sizes, no_payments)

  # M7: Distribution of payments over time
  payment_delays <- claim_payment_delay(
    freq_vector, claim_sizes, no_payments, setl_delays)
  payment_times <- claim_payment_time(
    freq_vector, occurrence_times, ntfy_delays, payment_delays)

  # M8: Claim inflation
  # Remove superimposed inflation
  payment_inflated <- claim_payment_inflation(
    freq_vector,
    payment_sizes,
    payment_times,
    occurrence_times,
    claim_sizes,
    base_inflation_vector = rep(params$base_rate, times = n_periods * 2),
    si_occurrence_function = params$si_occurrence_function,
    si_payment_funtion = params$si_payment_funtion
  )

  claims_inst <- claims(
    freq_vector, occurrence_times, claim_sizes, ntfy_delays, setl_delays,
    no_payments, payment_sizes, payment_delays, payment_times, payment_inflated)

  if ('claims' %in% data_type) {
    # Generate claim dataset
    output$claim_dataset <- generate_claim_dataset(
      frequency_vector = freq_vector,
      occurrence_list = occurrence_times,
      claim_size_list = claim_sizes,
      notification_list = ntfy_delays,
      settlement_list = setl_delays,
      no_payments_list = no_payments)
  }

  if ('payments' %in% data_type) {
    # Generate payment dataset
    output$payment_dataset <- generate_transaction_dataset(
      claims_inst,
      adjust = FALSE # to keep the original simulated payment times
    )
  }

  if ('incurred' %in% data_type) {
    # M9: Major revisions of incurred loss
    major <- claim_majRev_freq(claims_inst)
    major <- claim_majRev_time(claims_inst, major)
    major <- claim_majRev_size(major)

    # M10: Minor revisions of outstanding claim payments
    minor <- claim_minRev_freq(claims_inst)
    minor <- claim_minRev_time(claims_inst, minor)
    minor <- claim_minRev_size(claims_inst, major, minor)

    # M11: Development of case estimates (with inflation)
    incurred_history <- claim_history(
      claims_inst, major, minor,
      base_inflation_vector = rep(params$base_rate, times = n_periods * 2))

    # Generate incurred dataset
    output$incurred_dataset <- generate_incurred_dataset(
      claims_inst, incurred_history)
  }

  return(output)

}
