test_that("theta_option_types and theta_option_names align", {
  expect_setequal(
    names(theta_option_types),
    purrr::map_chr(
      ls(theta_option_names),
      ~ get(.x, envir = theta_option_names)
    )
  )
})

test_that("parse_theta_record() works", {
  cases <- list(
    list(
      input = "$THETA 1",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA  (1,2,3)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace("  "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA  (1,2  3)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace("  "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_whitespace("  "),
              option_pos$new("up", "3"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA 1 fix",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              option_pos$new("init", "1"),
              elem_whitespace(" "),
              option_flag$new("fixed", name_raw = "fix", value = TRUE)
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA 1 uni",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              option_pos$new("init", "1"),
              elem_whitespace(" "),
              option_flag$new("unint", name_raw = "uni", value = TRUE)
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA 1 unint, fixed",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              option_pos$new("init", "1"),
              elem_whitespace(" "),
              option_flag$new(
                "unint",
                name_raw = "unint", value = TRUE
              ),
              elem_comma(), elem_whitespace(" "),
              option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA  (1,,3)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace("  "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(), elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA (FIX 2)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_flag$new("fixed", name_raw = "FIX", value = TRUE),
              elem_whitespace(" "),
              option_pos$new("init", "2"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA  (1,2,3 fixed)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace("  "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_whitespace(" "),
              option_flag$new("fixed", name_raw = "fixed", value = TRUE),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$THETA (1,2,3) ; ok",
        "        fixed"
      ),
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close(),
              elem_whitespace(" "), elem_comment("; ok"), elem_linebreak(),
              elem_whitespace("        "),
              option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA (1,2,3) fixed numberpts=3",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close(),
              elem_whitespace(" "),
              option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          elem_whitespace(" "),
          option_value$new(
            "numberpoints",
            name_raw = "numberpts", value = "3", sep = "="
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$THETA (1,2,3) 4 ",
        "fixed",
        "(5 6)"
      ),
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              option_pos$new("init", "4"),
              elem_whitespace(" "),
              elem_linebreak(),
              option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          elem_linebreak(),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "5"),
              elem_whitespace(" "),
              option_pos$new("init", "6"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA foo=1",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_pos$new("label", value = "foo="),
          option_nested$new(
            "theta",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA foo= 1",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_pos$new("label", value = "foo="),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA 1E-3",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(option_pos$new("init", "1E-3"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA foo=(1,2)",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_pos$new("label", value = "foo="),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA NAMES(V1,CL,Q,V2) (0.0,5.0) (0.0,6.0) (0.0,7.0) 8",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_value$new(
            "names",
            name_raw = "NAMES", value = "(V1,CL,Q,V2)", sep = ""
          ),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", value = "0.0"),
              elem_comma(),
              option_pos$new("init", value = "5.0"),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", value = "0.0"),
              elem_comma(),
              option_pos$new("init", value = "6.0"),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", value = "0.0"),
              elem_comma(),
              option_pos$new("init", value = "7.0"),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(option_pos$new("init", value = "8"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA (1,2,3)x3",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close(),
              option_value$new("x", name_raw = "x", value = "3", sep = "")
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$THETA (1,2,3) X 3",
      want = list(
        values = list(
          option_record_name$new("theta", "THETA"),
          elem_whitespace(" "),
          option_nested$new(
            "theta",
            values = list(
              elem_paren_open(),
              option_pos$new("low", "1"),
              elem_comma(),
              option_pos$new("init", "2"),
              elem_comma(),
              option_pos$new("up", "3"),
              elem_paren_close(), elem_whitespace(" "),
              option_value$new("x", name_raw = "X", value = "3", sep = " ")
            )
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_theta$new("theta", "THETA", case$input)
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

test_that("theta records are combined", {
  lines <- c(
    "$prob",
    "$theta 1",
    "$data foo.csv",
    "$theta (2,3,4)"
  )

  ctl <- parse_ctl(lines)
  recs <- ctl$records

  for (i in c(2, 4)) {
    expect_null(recs[[i]]$values)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$values,
    list(
      option_record_name$new("theta", "theta"),
      elem_whitespace(" "),
      option_nested$new(
        "theta",
        values = list(option_pos$new("init", "1"))
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      option_record_name$new("theta", "theta"),
      elem_whitespace(" "),
      option_nested$new(
        "theta",
        values = list(
          elem_paren_open(),
          option_pos$new("low", "2"),
          elem_comma(),
          option_pos$new("init", "3"),
          elem_comma(),
          option_pos$new("up", "4"),
          elem_paren_close()
        )
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    format(ctl),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})

test_that("parsing theta aborts if value doesn't look like number", {
  cases <- list(
    "$theta THETA1",
    "$theta 1 THETA2 = THETA(1)",
    "$theta 1 (N)"
  )
  for (case in cases) {
    rec <- record_theta$new("theta", "theta", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})
