test_that("format.nmrec_ctl_records() works", {
  exs <- list(nmex_control3, nmex_ccontrs, nmex_bayes1)
  for (ex in exs) {
    expected <- paste0(paste0(ex, collapse = "\n"), "\n")
    recs <- parse_ctl(ex)
    expect_identical(format(recs), expected)

    # Still matches after $parse().
    data <- purrr::detect(recs$records, ~ .x[["name"]] == "data")
    if (is.null(data)) {
      abort(sprintf("missing $data record: %s", format(recs)))
    }
    data$parse()
    expect_identical(format(recs), expected)

    tfile <- withr::local_tempfile(pattern = "nmrec-tests-")
    write_ctl(recs, tfile)
    expect_identical(readLines(tfile), ex)
  }
})
