#############################################################################
##                    Datasets Included in This Package                    ##
#############################################################################

#' Incurred Case Estimates Dataset
#'
#' A dataset of 31,250 records of transactions (partial payments and incurred
#' revisions) associated with the 3,624 claims described in `SynthETIC`'s
#' \code{\link[SynthETIC]{test_claim_dataset}}. The `_inflated` version includes
#' inflation adjustment in the case estimates, while the `_noInf` version
#' excludes any inflation effects.
#'
#' @format A data frame with 31,250 rows and 9 variables:
#' \describe{
#'   \item{claim_no}{claim number, which uniquely characterises each claim.}
#'   \item{claim_size}{size of the claim (in constant dollar values).}
#'   \item{txn_time}{double; the "absolute" time of transaction (on a continuous
#'                   time scale).}
#'   \item{txn_delay}{delay from notification to the subject transaction.}
#'   \item{txn_type}{character; nature of the transactions, "Ma" for major
#'                   revision, "Mi" for minor revision, "P" for payment,
#'                   "PMa" for major revision coincident with a payment,
#'                   "PMi" for minor revision coincident with a payment.}
#'   \item{incurred}{double; case estimate of total incurred loss immediately
#'                   **after** the transaction.}
#'   \item{OCL}{double; case estimate of outstanding claim payments immediately
#'              **after** the transaction.}
#'   \item{cumpaid}{double; cumulative claim paid **after** the transaction.}
#'   \item{multiplier}{revision multipliers (subject to further constraints
#'                     documented in \code{\link{claim_history}}), `NA` for
#'                     transactions that do not involve a revision. Note that
#'                     major revision multipliers apply to the incurred losses,
#'                     while minor revision multipliers apply to the outstanding
#'                     claim payments.}
#' }
#' @seealso \code{\link{generate_incurred_dataset}}
#' @name test_incurred_dataset
"test_incurred_dataset_noInf"

#' @rdname test_incurred_dataset
#' @format NULL
"test_incurred_dataset_inflated"
