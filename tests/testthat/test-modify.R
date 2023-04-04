test_that("can modify control stream", {
  lines <- c(
    "$prob p",
    "$est meth=bayes niter=5 print=1"
  )

  recs <- parse_ctl(lines)
  est <- select_records(recs, "est")[[1]]

  expect_identical(
    format(recs),
    "$prob p\n$est meth=bayes niter=5 print=1\n"
  )

  niter <- get_record_option(est, "niter")
  niter$value <- 200

  expect_identical(
    format(recs),
    "$prob p\n$est meth=bayes niter=200 print=1\n"
  )

  method <- get_record_option(est, "method")
  method$name_raw <- "METHOD"
  method$sep <- " "

  expect_identical(
    format(recs),
    "$prob p\n$est METHOD bayes niter=200 print=1\n"
  )

  idx_last_space <- purrr::detect_index(
    est$values,
    ~ inherits(.x, "nmrec_whitespace"),
    .dir = "backward"
  )
  est$values[[idx_last_space]] <- elem_linebreak()

  expect_identical(
    format(recs),
    "$prob p\n$est METHOD bayes niter=200\nprint=1\n"
  )
})
