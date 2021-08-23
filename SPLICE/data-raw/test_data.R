###############################################################################
##                             Incurred Dataset                              ##
###############################################################################

## v2 incurred program
set.seed(20201006)
claims <- SynthETIC::test_claims_object
major <- claim_maRev_no(claims)
major <- claim_maRev_time(claims, major)
major <- claim_maRev_size(major)

# minor revisions
minor <- claim_miRev_no(claims)
minor <- claim_miRev_time(claims, minor)
minor <- claim_miRev_size(claims, major, minor)

test <- claim_history(claims, major, minor)
test_inflated <- claim_history(claims, major, minor, inflated = TRUE,
                               base_inflation_vector = rep((1 + 0.02)^(1/4) - 1, times = 80))
test_incurred_dataset <- generate_incurred_dataset(claims, test)
test_incurred_dataset_inflated <- generate_incurred_dataset(claims, test_inflated)

usethis::use_data(test_incurred_dataset, overwrite = TRUE)
usethis::use_data(test_incurred_dataset_inflated, overwrite = TRUE)

