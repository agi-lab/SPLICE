###############################################################################
##         Incurred Dataset with Default Covariates from SynthETIC           ##
###############################################################################

## v2 incurred program
set.seed(20201006)
test_claims <- SynthETIC::test_claims_object_cov
major <- claim_majRev_freq(test_claims)
major <- claim_majRev_time(test_claims, major)
major <- claim_majRev_size(major)

# minor revisions
minor <- claim_minRev_freq(test_claims)
minor <- claim_minRev_time(test_claims, minor)
minor <- claim_minRev_size(test_claims, major, minor)

test <- claim_history(test_claims, major, minor)
test_inflated <- claim_history(test_claims, major, minor,
                               base_inflation_vector = rep((1 + 0.02)^(1/4) - 1, times = 80))
test_incurred_dataset_noInf_cov <- generate_incurred_dataset(test_claims, test)
test_incurred_dataset_inflated_cov <- generate_incurred_dataset(test_claims, test_inflated)

usethis::use_data(test_incurred_dataset_noInf_cov, overwrite = TRUE)
usethis::use_data(test_incurred_dataset_inflated_cov, overwrite = TRUE)

