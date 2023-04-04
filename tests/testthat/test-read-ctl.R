test_that("parse_ctl() aborts if no records are found", {
  cases <- list(
    c(),
    "",
    "foo",
    c("foo", "bar")
  )
  for (case in cases) {
    expect_error(parse_ctl(!!case), class = "nmrec_parse_error")
  }
})

test_that("read_ctl() aborts if no records are found", {
  empty_file <- withr::local_tempfile(
    pattern = "nmrec-tests-",
    lines = character()
  )
  expect_error(read_ctl(empty_file), class = "nmrec_parse_error")
})

test_that("parse_ctl() warns on unknown record", {
  cases <- list(
    c("$PROBLEM", "$FOO"),
    c("$PROBLEM", "$FOO problem")
  )
  for (case in cases) {
    expect_warning(parse_ctl(!!case),
      "FOO",
      class = "nmrec_warning"
    )
  }
})

test_that("parse_ctl() aborts no $PROBLEM record is found", {
  cases <- list(
    "$DATA",
    c("; $PROBLEM", "$DATA")
  )
  for (case in cases) {
    expect_error(parse_ctl(!!case), class = "nmrec_parse_error")
  }
})

test_that("parse_ctl() aborts multiple $PROBLEM records are found", {
  cases <- list(
    c("$problem", "$PROBLEM"),
    c("$PROB", "$PROBLEM"),
    c("$PROB", "$DATA", "$PROB"),
    c("$problem", "  $problem")
  )
  for (case in cases) {
    expect_error(parse_ctl(!!case), class = "nmrec_unsupported")
  }
})

test_that("parse_ctl() aborts when INCLUDE is used", {
  cases <- list(
    c("$PROBLEM", "  $include"),
    c("INCLUDE"),
    # NM-TRAN works without a separator before the value.
    c("INCLUDEstuck")
  )
  for (case in cases) {
    expect_error(parse_ctl(!!case), class = "nmrec_unsupported")
  }
})

test_that("parse_ctl() works: control 3", {
  res <- parse_ctl(nmex_control3)
  expect_s3_class(res, "nmrec_ctl_records")
  records <- res$records
  expect_length(records, 13)
  for (i in seq_along(records)) {
    expect_s3_class(records[[!!i]], "nmrec_record")
  }

  rec <- records[[3]]
  expect_s3_class(rec, "nmrec_record_data")
  expect_identical(rec$name, "data")
  expect_identical(rec$get_lines(), "$DATA  DATA3")
  expect_identical(rec$format(), "$DATA  DATA3\n")

  rec <- records[[11]]
  expect_s3_class(rec, "nmrec_record_raw")
  expect_identical(rec$name, "scatterplot")
  expect_identical(rec$get_lines(), "$SCAT    CP VS TIME")
  expect_identical(rec$format(), "$SCAT    CP VS TIME\n")
  expect_error(rec$parse(), class = "nmrec_unsupported")
})

test_that("parse_ctl() works: ccontrs", {
  res <- parse_ctl(nmex_ccontrs)
  expect_s3_class(res, "nmrec_ctl_records")
  records <- res$records
  expect_length(records, 10)
  for (i in seq_along(records)) {
    expect_s3_class(records[[!!i]], "nmrec_record")
  }

  rec <- records[[1]]
  expect_identical(rec$name, "problem")
  expect_identical(rec$get_lines(), "$PROBLEM")

  rec <- records[[4]]
  expect_identical(rec$name, "subroutines")
  expect_identical(rec$get_lines(), "$SUB CONTR=CONTR.txt CCONTR=CCONTR.txt")
})

test_that("parse_ctl() works: bayes1", {
  res <- parse_ctl(nmex_bayes1)
  expect_s3_class(res, "nmrec_ctl_records")
  records <- res$records
  expect_length(records, 25)
  for (i in seq_along(records)) {
    expect_s3_class(records[[!!i]], "nmrec_record")
  }

  rec <- records[[3]]
  expect_s3_class(rec, "nmrec_record_data")
  expect_identical(rec$name, "data")
  expect_identical(rec$get_lines(), "$DATA example1.csv IGNORE=C")
  expect_null(rec$values)
  expect_identical(rec$format(), "$DATA example1.csv IGNORE=C\n")
  rec$parse()
  expect_identical(
    purrr::map_chr(rec$get_options(), "name"),
    c("data", "filename", "ignore")
  )
  expect_identical(rec$format(), "$DATA example1.csv IGNORE=C\n")
})

test_that("make_record() dev error on empty lines", {
  expect_error(make_record("name", "name_raw", c()),
    class = "nmrec_dev_error"
  )
})

test_that("extract_record_name() dev errors", {
  cases <- list(
    "",
    "nodollar"
  )
  for (case in cases) {
    expect_error(extract_record_name(!!case),
      class = "nmrec_dev_error"
    )
  }
})
