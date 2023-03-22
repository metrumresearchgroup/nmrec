test_that("estimation_option_types and estimation_option_names align", {
  expect_setequal(
    names(estimation_option_types),
    purrr::map_chr(
      ls(estimation_option_names),
      ~ get(.x, envir = estimation_option_names)
    )
  )
})

test_that("parse_estimation_record() works", {
  cases <- list(
    list(
      input = c(
        "$est max 888 ; comment",
        "    ; other comment",
        "    nsample= 99"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          elem_comment("; comment"), elem_linebreak(),
          elem_whitespace("    "),
          elem_comment("; other comment"), elem_linebreak(),
          elem_whitespace("    "), 2L, elem_linebreak()
        ),
        options = list(
          maxevals = option_value$new(
            "maxevals",
            name_raw = "max", value = "888", sep = " "
          ),
          niter = option_value$new(
            "niter",
            name_raw = "nsample", value = "99", sep = "= "
          )
        )
      )
    )
  )

  for (case in cases) {
    res <- parse_estimation_record("est", case$input)
    expect_identical(!!res, !!case$want)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("est", res$template, res$options),
      paste0(
        paste0(!!case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})
