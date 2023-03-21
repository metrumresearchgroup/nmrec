test_that("print.nmrec_ctl_records() works", {
  recs <- parse_ctl(nmex_control3)
  expect_output(print(recs), "$DATA", fixed = TRUE)
})
