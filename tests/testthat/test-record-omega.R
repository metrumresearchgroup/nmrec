test_that("parse_omega_record() works", {
  cases <- list(
    list(
      input = "$OMEGA 1",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          )
        )
      )
    ),
    list(
      input = "$OMEGA diag(1) 1",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), 2L, elem_linebreak()
        ),
        options = list(
          diagonal = option_value$new(
            "diagonal", "diag", "(1)",
            sep = ""
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          )
        )
      )
    ),
    list(
      input = c(
        "$OMEGA 1 FIX sd ; c",
        "  2"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), elem_comment("; c"), elem_linebreak(),
          elem_whitespace("  "), 2L, elem_linebreak()
        ),
        options = list(
          omega = option_param$new(
            "omega",
            template = list(
              1L, elem_whitespace(" "),
              2L, elem_whitespace(" "),
              3L
            ),
            values = list(
              init = option_pos$new("init", "1"),
              fixed = option_flag$new("fixed", "FIX"),
              standard = option_flag$new("standard", "sd")
            )
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(
              init = option_pos$new("init", "2")
            )
          )
        )
      )
    ),
    list(
      input = "$OMEGA 1,(fix 2) (3 uni,SD)",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_comma(),
          2L, elem_whitespace(" "),
          3L, elem_linebreak()
        ),
        options = list(
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          ),
          omega = option_param$new(
            "omega",
            template = list(
              elem_paren_open(), 1L, elem_whitespace(" "),
              2L, elem_paren_close()
            ),
            values = list(
              fixed = option_flag$new("fixed", "fix"),
              init = option_pos$new("init", "2")
            )
          ),
          omega = option_param$new(
            "omega",
            template = list(
              elem_paren_open(), 1L, elem_whitespace(" "),
              2L, elem_comma(),
              3L, elem_paren_close()
            ),
            values = list(
              init = option_pos$new("init", "3"),
              unint = option_flag$new("unint", "uni"),
              standard = option_flag$new("standard", "SD")
            )
          )
        )
      )
    ),
    list(
      input = "$OMEGA 1 (2 3) X 4",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_linebreak()
        ),
        options = list(
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          ),
          omega = option_param$new(
            "omega",
            template = list(
              elem_paren_open(), 1L, elem_whitespace(" "),
              2L, elem_paren_close(), elem_whitespace(" "),
              3L
            ),
            values = list(
              init = option_pos$new("init", "2"),
              init = option_pos$new("init", "3"),
              x = option_value$new("x", "X", "4", sep = " ")
            )
          )
        )
      )
    ),
    list(
      input = "$OMEGA bloc(2) SAME",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), 2L, elem_linebreak()
        ),
        options = list(
          block = option_value$new(
            "block", "bloc", "(2)",
            sep = ""
          ),
          same = option_flag$new("same", "SAME")
        )
      )
    ),
    list(
      input = "$OMEGA BLOCK(2) same(3)",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), 2L, elem_linebreak()
        ),
        options = list(
          block = option_value$new("block", "BLOCK", "(2)", sep = ""),
          same = option_value$new("same", "same", "(3)", sep = "")
        )
      )
    ),
    list(
      input = "$OMEGA BLOCK (2) FIX VAL (0.1,0.01)",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak()
        ),
        options = list(
          block = option_value$new(
            "block", "BLOCK", "(2)",
            sep = " "
          ),
          fixed = option_flag$new("fixed", "FIX"),
          values = option_value$new("values", "VAL", "(0.1,0.01)", sep = " ")
        )
      )
    ),
    list(
      input = c(
        "$OMEGA sd CORRELATION BLOCK(2)",
        "0.8",
        "-0.24 0.58"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak(),
          4L, elem_linebreak(),
          5L, elem_whitespace(" "), 6L, elem_linebreak()
        ),
        options = list(
          standard = option_flag$new("standard", "sd"),
          correlation = option_flag$new("correlation", "CORRELATION"),
          block = option_value$new("block", "BLOCK", "(2)", sep = ""),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.8"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "-0.24"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.58"))
          )
        )
      )
    ),
    list(
      input = c(
        "$OMEGA",
        "foo=0.1",
        "bar= (0.2 fix)"
      ),
      want = list(
        template = list(
          "record_name", elem_linebreak(),
          1L, 2L, elem_linebreak(),
          3L, elem_whitespace(" "), 4L, elem_linebreak()
        ),
        options = list(
          label = option_pos$new("label", value = "foo="),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          ),
          label = option_pos$new("label", value = "bar="),
          omega = option_param$new(
            "omega",
            template = list(
              elem_paren_open(), 1L, elem_whitespace(" "),
              2L, elem_paren_close()
            ),
            values = list(
              init = option_pos$new("init", "0.2"),
              fixed = option_flag$new("fixed", "fix")
            )
          )
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(4)",
        "ECL=  0.1",
        "EV1=  0.01 0.35",
        "EQ=   0.01 0.01 0.54",
        "EV2=  0.01 0.01 0.01 0.67"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak(),
          2L, elem_whitespace("  "), 3L,
          elem_linebreak(),
          4L, elem_whitespace("  "), 5L, elem_whitespace(" "), 6L,
          elem_linebreak(),
          7L, elem_whitespace("   "), 8L, elem_whitespace(" "), 9L, elem_whitespace(" "), 10L,
          elem_linebreak(),
          11L, elem_whitespace("  "), 12L, elem_whitespace(" "), 13L, elem_whitespace(" "), 14L, elem_whitespace(" "), 15L,
          elem_linebreak()
        ),
        options = list(
          block = option_value$new("block", "BLOCK", "(4)", sep = ""),
          label = option_pos$new("label", value = "ECL="),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          ),
          label = option_pos$new("label", value = "EV1="),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.35"))
          ),
          label = option_pos$new("label", value = "EQ="),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.54"))
          ),
          label = option_pos$new("label", value = "EV2="),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.67"))
          )
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(4)",
        "0.1",
        "0.01 0.1",
        "(0.01)x2 0.1",
        "(0.01)x3 0.1"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_linebreak(),
          2L, elem_linebreak(),
          3L, elem_whitespace(" "), 4L, elem_linebreak(),
          5L, elem_whitespace(" "), 6L, elem_linebreak(),
          7L, elem_whitespace(" "), 8L, elem_linebreak()
        ),
        options = list(
          block = option_value$new("block", "BLOCK", "(4)", sep = ""),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.01"))
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          ),
          omega = option_param$new(
            "omega",
            template = list(elem_paren_open(), 1L, elem_paren_close(), 2L),
            values = list(
              init = option_pos$new("init", "0.01"),
              x = option_value$new("x", "x", "2", sep = "")
            )
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          ),
          omega = option_param$new(
            "omega",
            template = list(elem_paren_open(), 1L, elem_paren_close(), 2L),
            values = list(
              init = option_pos$new("init", "0.01"),
              x = option_value$new("x", "x", "3", sep = "")
            )
          ),
          omega = option_param$new(
            "omega",
            template = list(1L),
            values = list(init = option_pos$new("init", "0.1"))
          )
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case$input)
    rec$parse()
    expect_identical(rec$template, case$want$template)
    expect_identical(rec$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("OMEGA", rec$template, rec$options),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})

test_that("omega records are combined", {
  lines <- c(
    "$prob",
    "$omega 1",
    "$data foo.csv",
    "$omega block(2)",
    "0.1",
    "0.01 0.2"
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
      omega = option_param$new(
        "omega",
        template = list(1L),
        values = list(init = option_pos$new("init", "1"))
      )
    )
  )

  expect_identical(
    recs[[4]]$template,
    list(
      "record_name", elem_whitespace(" "),
      1L, elem_linebreak(),
      2L, elem_linebreak(),
      3L, elem_whitespace(" "), 4L, elem_linebreak()
    )
  )
  expect_identical(
    recs[[4]]$options,
    list(
      block = option_value$new("block", "block", "(2)", sep = ""),
      omega = option_param$new(
        "omega",
        template = list(1L),
        values = list(init = option_pos$new("init", "0.1"))
      ),
      omega = option_param$new(
        "omega",
        template = list(1L),
        values = list(init = option_pos$new("init", "0.01"))
      ),
      omega = option_param$new(
        "omega",
        template = list(1L),
        values = list(init = option_pos$new("init", "0.2"))
      )
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
