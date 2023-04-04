test_that("get_record_option() can get a option_pos option", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  data_fnames <- purrr::map(
    select_records(ctl, "data"),
    ~ get_record_option(.x, "filename")
  )
  expect_identical(
    data_fnames,
    list(option_pos$new("filename", value = "example1.csv"))
  )
})

test_that("get_record_option() supports abbreviated options", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  methods <- purrr::map(
    select_records(ctl, "est"),
    ~ get_record_option(.x, "meth")
  )
  expect_identical(
    methods,
    list(
      option_value$new("method", "METHOD", value = "ITS", sep = "="),
      option_value$new("method", "METHOD", value = "SAEM", sep = "="),
      option_value$new("method", "METHOD", value = "IMP", sep = "="),
      option_value$new("method", "METHOD", value = "BAYES", sep = "="),
      option_value$new("method", "METHOD", value = "COND", sep = "=")
    )
  )
})

test_that("get_record_option() supports matrix prefix options", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  blocks <- purrr::map(
    select_records(ctl, "omega"),
    ~ get_record_option(.x, "block")
  )
  expect_identical(
    blocks,
    list(option_value$new("block", "BLOCK", value = "(4)", sep = ""))
  )
})

test_that("get_record_option() errors if more than one option is found", {
  ctl <- parse_ctl(get("a_uflg", envir = nmrec_examples))
  data <- select_records(ctl, "data")
  expect_error(get_record_option(data[[1]], "ign"), class = "nmrec_error")
})

test_that("get_record_option() errors if no option is found", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))
  tabs <- select_records(ctl, "table")
  expect_error(get_record_option(tabs[[1]], "wres"), class = "nmrec_error")
})

test_that("get_record_option() errors on unsupported record type", {
  ctl <- parse_ctl("$prob p")
  expect_error(get_record_option(ctl$records[[1]], "foo"),
    class = "nmrec_unsupported"
  )
})
