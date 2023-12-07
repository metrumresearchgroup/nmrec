test_that("can modify control stream", {
  lines <- c(
    "$prob p",
    "$est meth=bayes niter=5 print=1"
  )

  ctl <- parse_ctl(lines)
  est <- select_records(ctl, "est")[[1]]

  expect_identical(
    format(ctl),
    "$prob p\n$est meth=bayes niter=5 print=1\n"
  )

  niter <- get_record_option(est, "niter")
  niter$value <- 200

  expect_identical(
    format(ctl),
    "$prob p\n$est meth=bayes niter=200 print=1\n"
  )

  method <- get_record_option(est, "method")
  method$name_raw <- "METHOD"
  method$sep <- " "

  expect_identical(
    format(ctl),
    "$prob p\n$est METHOD bayes niter=200 print=1\n"
  )

  idx_last_space <- purrr::detect_index(
    est$values,
    function(x) inherits(x, "nmrec_whitespace"),
    .dir = "backward"
  )
  est$values[[idx_last_space]] <- elem_linebreak()

  expect_identical(
    format(ctl),
    "$prob p\n$est METHOD bayes niter=200\nprint=1\n"
  )

  prob_text <- get_record_option(ctl$records[[1]], "text")
  prob_text$value <- "foo"
  expect_identical(
    format(ctl),
    "$prob foo\n$est METHOD bayes niter=200\nprint=1\n"
  )
})

test_that("can insert string for a record", {
  ctl <- parse_ctl(c("$prob p", "$theta 1", "$omega 2"))
  ctl$records[[2]] <- "$theta 3\n"
  expect_identical(format(ctl), "$prob p\n$theta 3\n$omega 2\n")
})
