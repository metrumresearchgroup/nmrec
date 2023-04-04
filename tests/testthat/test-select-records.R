test_that("select_records() works", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  ests <- select_records(ctl, "est")
  expect_length(ests, 5)
  for (est in ests) {
    expect_s3_class(est, "nmrec_record_estimation")
  }

  thetas <- select_records(ctl, "the")
  expect_length(thetas, 1)
  expect_s3_class(thetas[[1]], "nmrec_record_theta")
})
