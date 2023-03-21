test_that("format.nmrec_ctl_records() works", {
  exs <- list(nmex_control3, nmex_ccontrs, nmex_bayes1, nmex_a_uflg)
  for (ex in exs) {
    expected <- paste0(paste0(ex, collapse = "\n"), "\n")
    recs <- parse_ctl(ex)
    expect_identical(format(recs), expected)

    # Still matches after $parse().
    parsed_something <- FALSE
    for (r in recs$records) {
      if (!inherits(r, "nmrec_record_raw")) {
        r$parse()
        parsed_something <- TRUE
      }
    }
    if (!parsed_something) {
      abort("failed to trigger parse() for test")
    }

    expect_identical(format(recs), expected)

    tfile <- withr::local_tempfile(pattern = "nmrec-tests-")
    write_ctl(recs, tfile)
    expect_identical(readLines(tfile), ex)
  }
})
