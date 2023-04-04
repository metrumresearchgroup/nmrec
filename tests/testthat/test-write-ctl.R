test_that("format.nmrec_ctl_records() works", {
  exs <- list(nmex_control3, nmex_ccontrs, nmex_bayes1, nmex_a_uflg)
  for (ex in exs) {
    expected <- paste0(paste0(ex, collapse = "\n"), "\n")
    ctl <- parse_ctl(ex)
    expect_identical(format(ctl), expected)

    # Still matches after $parse().
    parsed_something <- FALSE
    for (r in ctl$records) {
      if (!inherits(r, "nmrec_record_raw")) {
        r$parse()
        parsed_something <- TRUE
      }
    }
    if (!parsed_something) {
      abort("failed to trigger parse() for test")
    }

    expect_identical(format(ctl), expected)

    tfile <- withr::local_tempfile(pattern = "nmrec-tests-")
    write_ctl(ctl, tfile)
    expect_identical(readLines(tfile), ex)
  }
})

test_that("format() method preserves whitespace before records", {
  lines <- c(
    "$prob p",
    " \t  $data a.csv"
  )
  ctl <- parse_ctl(lines)

  expected <- paste0(paste(lines, collapse = "\n"), "\n")
  expect_identical(format(ctl), expected)
  ctl$records[[2]]$parse()
  expect_identical(format(ctl), expected)
})
