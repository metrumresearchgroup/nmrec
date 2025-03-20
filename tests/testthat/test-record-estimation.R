test_that("estimation_option_types and estimation_option_names align", {
  expect_setequal(
    names(estimation_option_types),
    purrr::map_chr(
      ls(estimation_option_names),
      function(x) get(x, envir = estimation_option_names)
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
        values = list(
          option_record_name$new("estimation", "est"),
          elem_whitespace(" "),
          option_value$new(
            "maxevals",
            name_raw = "max", value = "888", sep = " "
          ),
          elem_whitespace(" "),
          elem_comment("; comment"), elem_linebreak(),
          elem_whitespace("    "),
          elem_comment("; other comment"), elem_linebreak(),
          elem_whitespace("    "),
          option_value$new(
            "niter",
            name_raw = "nsample", value = "99", sep = "= "
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$est maxeval=0 evalshrink 1",
      want = list(
        values = list(
          option_record_name$new("estimation", "est"),
          elem_whitespace(" "),
          option_value$new(
            "maxevals",
            name_raw = "maxeval", value = "0", sep = "="
          ),
          elem_whitespace(" "),
          option_value$new(
            "evalshrink",
            name_raw = "evalshrink", value = "1", sep = " "
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$est meth 1 format =,1PE12.5",
      want = list(
        values = list(
          option_record_name$new("estimation", "est"),
          elem_whitespace(" "),
          option_value$new(
            "method",
            name_raw = "meth", value = "1", sep = " "
          ),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format", value = ",1PE12.5", sep = " ="
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$est meth 1 delim=,1PE12.5",
      want = list(
        values = list(
          option_record_name$new("estimation", "est"),
          elem_whitespace(" "),
          option_value$new(
            "method",
            name_raw = "meth", value = "1", sep = " "
          ),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "delim", value = ",1PE12.5", sep = "="
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_estimation$new("estimation", "est", case$input)
    rec$parse()
    expect_false(is.null(rec$values))
    expect_identical(rec$values, case$want$values)
    # Inputs and results match when rendered as string.
    expect_identical(
      rec$format(),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})
