test_that("parse_problem_record() aborts if text is on subsequent lines", {
  cases <- list(
    c("$problem", "foo"),
    c("$problem foo", "bar"),
    c("$problem foo", "", "bar")
  )
  for (case in cases) {
    rec <- record_problem$new("problem", "problem", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})

test_that("parse_problem_record() works", {
  cases <- list(
    list(
      input = "$problem",
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "  $problem",
      want = list(
        values = list(
          elem_whitespace("  "),
          option_record_name$new("problem", "problem"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c("$problem", "", "\t; comment"),
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_linebreak(),
          elem_linebreak(),
          elem_whitespace("\t"),
          elem_comment("; comment"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$problem ",
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_whitespace(" "),
          option_pos$new("text", value = ""),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$problem foo",
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_whitespace(" "),
          option_pos$new("text", value = "foo"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$problem foo ; bar",
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_whitespace(" "),
          option_pos$new("text", value = "foo ; bar"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c("$problem foo", "; bar"),
      want = list(
        values = list(
          option_record_name$new("problem", "problem"),
          elem_whitespace(" "),
          option_pos$new("text", value = "foo"),
          elem_linebreak(),
          elem_comment("; bar"),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_problem$new("problem", "problem", case$input)
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
