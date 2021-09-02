###############################################################################
##                             Incurred Dataset                              ##
###############################################################################

## v2 incurred program
set.seed(20201006)
test_claims <- SynthETIC::test_claims_object
major <- claim_maRev_no(test_claims)
major <- claim_maRev_time(test_claims, major)
major <- claim_maRev_size(major)

# minor revisions
minor <- claim_miRev_no(test_claims)
minor <- claim_miRev_time(test_claims, minor)
minor <- claim_miRev_size(test_claims, major, minor)

test <- claim_history(test_claims, major, minor)
test_inflated <- claim_history(test_claims, major, minor,
                               base_inflation_vector = rep((1 + 0.02)^(1/4) - 1, times = 80))
test_incurred_dataset_noInf <- generate_incurred_dataset(test_claims, test)
test_incurred_dataset_inflated <- generate_incurred_dataset(test_claims, test_inflated)

usethis::use_data(test_incurred_dataset_noInf, overwrite = TRUE)
usethis::use_data(test_incurred_dataset_inflated, overwrite = TRUE)

