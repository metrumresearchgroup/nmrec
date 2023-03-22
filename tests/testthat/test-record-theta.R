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
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          )
        )
      )
    ),
    list(
      input = "$THETA  (1,2,3)",
      want = list(
        template = list(
          "record_name", elem_whitespace("  "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA  (1,2  3)",
      want = list(
        template = list(
          "record_name", elem_whitespace("  "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(),
              2L, elem_whitespace("  "),
              3L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA 1 fix",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(1L, elem_whitespace(" "), 2L),
            values = list(
              init = option_pos$new("init", "1"),
              fixed = option_flag$new("fixed", name_raw = "fix", value = TRUE)
            )
          )
        )
      )
    ),
    list(
      input = "$THETA 1 uni",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(1L, elem_whitespace(" "), 2L),
            values = list(
              init = option_pos$new("init", "1"),
              unint = option_flag$new("unint", name_raw = "uni", value = TRUE)
            )
          )
        )
      )
    ),
    list(
      input = "$THETA 1 unint, fixed",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              1L, elem_whitespace(" "),
              2L, elem_comma(), elem_whitespace(" "),
              3L
            ),
            values = list(
              init = option_pos$new("init", "1"),
              unint = option_flag$new(
                "unint",
                name_raw = "unint", value = TRUE
              ),
              fixed = option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          )
        )
      )
    ),
    list(
      input = "$THETA  (1,,3)",
      want = list(
        template = list(
          "record_name", elem_whitespace("  "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(), elem_comma(),
              2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              up = option_pos$new("up", "3")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA (FIX 2)",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_whitespace(" "),
              2L, elem_paren_close()
            ),
            values = list(
              fixed = option_flag$new("fixed", name_raw = "FIX", value = TRUE),
              init = option_pos$new("init", "2")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA  (1,2,3 fixed)",
      want = list(
        template = list(
          "record_name", elem_whitespace("  "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_whitespace(" "),
              4L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3"),
              fixed = option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          )
        )
      )
    ),
    list(
      input = c(
        "$THETA (1,2,3) ; ok",
        "        fixed"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close(),
              elem_whitespace(" "), elem_comment("; ok"), elem_linebreak(),
              elem_whitespace("        "), 4L
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3"),
              fixed = option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          )
        )
      )
    ),
    list(
      input = "$THETA (1,2,3) fixed numberpts=3",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), 2L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close(),
              elem_whitespace(" "), 4L
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3"),
              fixed = option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          numberpoints = option_value$new(
            "numberpoints",
            name_raw = "numberpts", value = "3", sep = "="
          )
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
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_linebreak(),
          3L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3")
            )
          ),
          theta = option_param$new(
            "theta",
            template = list(
              1L, elem_whitespace(" "), elem_linebreak(), 2L
            ),
            values = list(
              init = option_pos$new("init", "4"),
              fixed = option_flag$new("fixed", name_raw = "fixed", value = TRUE)
            )
          ),
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_whitespace(" "),
              2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "5"),
              init = option_pos$new("init", "6")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA foo=1",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, 2L, elem_linebreak()
        ),
        options = list(
          label = option_pos$new("label", value = "foo="),
          theta = option_param$new(
            "theta",
            template = list(1L),
            values = list(
              init = option_pos$new("init", "1")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA foo= 1",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), 2L, elem_linebreak()
        ),
        options = list(
          label = option_pos$new("label", value = "foo="),
          theta = option_param$new(
            "theta",
            template = list(1L),
            values = list(
              init = option_pos$new("init", "1")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA foo=(1,2)",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, 2L, elem_linebreak()
        ),
        options = list(
          label = option_pos$new("label", value = "foo="),
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(), 2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA NAMES(V1,CL,Q,V2) (0.0,5.0) (0.0,6.0) (0.0,7.0) 8",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_whitespace(" "),
          4L, elem_whitespace(" "),
          5L, elem_linebreak()
        ),
        options = list(
          names = option_value$new(
            "names",
            name_raw = "NAMES", value = "(V1,CL,Q,V2)", sep = ""
          ),
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(), 2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", value = "0.0"),
              init = option_pos$new("init", value = "5.0")
            )
          ),
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(), 2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", value = "0.0"),
              init = option_pos$new("init", value = "6.0")
            )
          ),
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(), 1L, elem_comma(), 2L, elem_paren_close()
            ),
            values = list(
              low = option_pos$new("low", value = "0.0"),
              init = option_pos$new("init", value = "7.0")
            )
          ),
          theta = option_param$new(
            "theta",
            template = list(1L),
            values = list(
              init = option_pos$new("init", value = "8")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA (1,2,3)x3",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close(),
              4L
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3"),
              x = option_value$new("x", name_raw = "x", value = "3", sep = "")
            )
          )
        )
      )
    ),
    list(
      input = "$THETA (1,2,3) X 3",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          theta = option_param$new(
            "theta",
            template = list(
              elem_paren_open(),
              1L, elem_comma(),
              2L, elem_comma(),
              3L, elem_paren_close(), elem_whitespace(" "),
              4L
            ),
            values = list(
              low = option_pos$new("low", "1"),
              init = option_pos$new("init", "2"),
              up = option_pos$new("up", "3"),
              x = option_value$new("x", name_raw = "X", value = "3", sep = " ")
            )
          )
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_theta$new("theta", "THETA", case$input)
    res <- rec$parse()
    expect_identical(res$template, case$want$template)
    expect_identical(res$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("THETA", res$template, res$options),
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

  res <- parse_ctl(lines)
  recs <- res$records

  for (i in c(2, 4)) {
    expect_null(recs[[i]]$template)
    expect_null(recs[[i]]$options)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[2]]$options,
    list(
      theta = option_param$new(
        "theta",
        template = list(1L),
        values = list(init = option_pos$new("init", "1"))
      )
    )
  )

  expect_identical(
    recs[[4]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[4]]$options,
    list(
      theta = option_param$new(
        "theta",
        template = list(
          elem_paren_open(), 1L, elem_comma(),
          2L, elem_comma(),
          3L, elem_paren_close()
        ),
        values = list(
          low = option_pos$new("low", "2"),
          init = option_pos$new("init", "3"),
          up = option_pos$new("up", "4")
        )
      )
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
