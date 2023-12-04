test_that("get_record_option() can get a option_pos option", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  recs <- select_records(ctl, "data")
  expect_length(recs, 1)
  rec <- recs[[1]]

  want <- option_pos$new("filename", value = "example1.csv")

  expect_identical(
    get_record_option(rec, "filename"),
    want
  )

  expect_identical(
    get_record_option(rec, "FILENAME"),
    want
  )
})

test_that("get_record_option() supports abbreviated options", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  methods <- purrr::map(
    select_records(ctl, "est"),
    function(x) get_record_option(x, "meth")
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
    function(x) get_record_option(x, "block")
  )
  expect_identical(
    blocks,
    list(option_value$new("block", "BLOCK", value = "(4)", sep = ""))
  )
})

test_that("get_record_option() errors if more than one option is found", {
  ctl <- parse_ctl(get("a_uflg", envir = nmrec_examples))
  data <- select_records(ctl, "data")
  expect_error(
    get_record_option(data[[1]], "ign"),
    "ign option",
    class = "nmrec_error",
  )
})

test_that("get_record_option() returns NULL if no option is found", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))
  tabs <- select_records(ctl, "table")
  expect_null(get_record_option(tabs[[1]], "wres"))
})

test_that("get_record_option() errors on unsupported record type", {
  ctl <- parse_ctl(c("$prob p", "$pk not supported"))
  expect_error(get_record_option(ctl$records[[2]], "foo"),
    class = "nmrec_unsupported"
  )
})
