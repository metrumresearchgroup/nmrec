test_that("print.nmrec_ctl_records() works", {
  ctl <- parse_ctl(nmex_control3)
  expect_output(print(ctl), "$DATA", fixed = TRUE)
})
