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

test_that("parse_ctl() works: control 3", {
  res <- parse_ctl(nmex_control3)
  expect_s3_class(res, "nmrec_ctl_records")
  records <- res$records
  expect_length(records, 13)
  for (i in seq_along(records)) {
    expect_s3_class(!!records[[i]], "nmrec_record")
  }

  rec <- records[[3]]
  expect_identical(rec$name, "data")
  expect_identical(rec$get_lines(), "$DATA  DATA3")
  expect_identical(rec$format(), "$DATA  DATA3\n")

  rec <- records[[7]]
  expect_s3_class(rec, "nmrec_record_raw")
  expect_identical(rec$name, "theta")
  expect_identical(rec$get_lines(), c("$THETA  (0,1.7)  (0,.102)  (0,29)", ""))
  expect_identical(rec$format(), "$THETA  (0,1.7)  (0,.102)  (0,29)\n\n")
})

test_that("parse_ctl() works: ccontrs", {
  res <- parse_ctl(nmex_ccontrs)
  expect_s3_class(res, "nmrec_ctl_records")
  records <- res$records
  expect_length(records, 10)
  for (i in seq_along(records)) {
    expect_s3_class(!!records[[i]], "nmrec_record")
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
    expect_s3_class(!!records[[i]], "nmrec_record")
  }

  rec <- records[[3]]
  expect_identical(rec$name, "data")
  expect_identical(rec$get_lines(), "$DATA example1.csv IGNORE=C")

  rec <- records[[10]]
  expect_s3_class(rec, "nmrec_record_raw")
  expect_identical(rec$name, "prior")
  expect_identical(
    rec$get_lines(),
    c(
      "$PRIOR NWPRI",
      "",
      "; Prior information of THETAS"
    )
  )
})

test_that("make_record() dev errors", {
  cases <- list(
    c(),
    "",
    c("", ""),
    "nodollar",
    c("", "nodollar")
  )
  for (case in cases) {
    expect_error(make_record(!!case), class = "nmrec_dev_error")
  }
})
