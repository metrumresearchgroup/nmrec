test_that("parse_omega_record() works", {
  cases <- list(
    list(
      input = "$OMEGA 1",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA diag(1) 1",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new(
            "diagonal", "diag", "(1)",
            sep = ""
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA 1 FIX sd ; c",
        "  2"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_pos$new("init", "1"),
              elem_whitespace(" "),
              option_flag$new("fixed", "FIX"),
              elem_whitespace(" "),
              option_flag$new("standard", "sd")
            )
          ),
          elem_whitespace(" "), elem_comment("; c"), elem_linebreak(),
          elem_whitespace("  "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "2"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 1,(fix 2) (3 uni,SD)",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "1"))
          ),
          elem_comma(),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_flag$new("fixed", "fix"),
              elem_whitespace(" "),
              option_pos$new("init", "2"),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "3"),
              elem_whitespace(" "),
              option_flag$new("unint", "uni"),
              elem_comma(),
              option_flag$new("standard", "SD"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 1 (2 3) X 4",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "1"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "2"),
              elem_whitespace(" "),
              option_pos$new("init", "3"),
              elem_paren_close(), elem_whitespace(" "),
              option_value$new("x", "X", "4", sep = " ")
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA bloc(2) SAME",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new(
            "block", "bloc", "(2)",
            sep = ""
          ),
          elem_whitespace(" "),
          option_flag$new("same", "SAME"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA BLOCK(2) same(3)",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(2)", sep = ""),
          elem_whitespace(" "),
          option_value$new("same", "same", "(3)", sep = ""),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA BLOCK (2) FIX VAL (0.1,0.01)",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new(
            "block", "BLOCK", "(2)",
            sep = " "
          ),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_flag$new("values", "VAL"),
              elem_whitespace(" "),
              elem_paren_open(),
              option_pos$new("diag", "0.1"),
              elem_comma(),
              option_pos$new("odiag", "0.01"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA block(4) values( 0.1 , 0.01 ) ; c",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new(
            "block", "block", "(4)",
            sep = ""
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_flag$new("values", "values"),
              elem_paren_open(),
              elem_whitespace(" "),
              option_pos$new("diag", "0.1"),
              elem_whitespace(" "),
              elem_comma(),
              elem_whitespace(" "),
              option_pos$new("odiag", "0.01"),
              elem_whitespace(" "),
              elem_paren_close()
            )
          ),
          elem_whitespace(" "),
          elem_comment("; c"),
          elem_linebreak()
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
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_flag$new("standard", "sd"),
          elem_whitespace(" "),
          option_flag$new("correlation", "CORRELATION"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(2)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.8"))
          ),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "-0.24"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.58"))
          ),
          elem_linebreak()
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
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_linebreak(),
          option_pos$new("label", value = "foo="),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_pos$new("label", value = "bar="),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "0.2"),
              elem_whitespace(" "),
              option_flag$new("fixed", "fix"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
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
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_linebreak(),
          option_pos$new("label", value = "ECL="),
          elem_whitespace("  "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_pos$new("label", value = "EV1="),
          elem_whitespace("  "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.35"))
          ),
          elem_linebreak(),
          option_pos$new("label", value = "EQ="),
          elem_whitespace("   "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.54"))
          ),
          elem_linebreak(),
          option_pos$new("label", value = "EV2="),
          elem_whitespace("  "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.67"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(4)",
        "0.1",
        "0.01 0.1",
        "(0.01)x2 0.1",
        "(0.01)X3 0.1"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "0.01"),
              elem_paren_close(),
              option_value$new("x", "x", "2", sep = "")
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "0.01"),
              elem_paren_close(),
              option_value$new("x", "X", "3", sep = "")
            )
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA (0.0225 UNINT)X4",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              elem_paren_open(),
              option_pos$new("init", "0.0225"),
              elem_whitespace(" "),
              option_flag$new("unint", "UNINT"),
              elem_paren_close(),
              option_value$new("x", "X", "4", sep = "")
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 0.5 0.7 SCALE(2.0) 0.8 0.9 sca(1.5) 0.1 0.2",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.5"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.7"))
          ),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(2.0)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.8"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.9"))
          ),
          elem_whitespace(" "),
          option_value$new("scale", "sca", "(1.5)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.2"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 0.5 0.7 fix SD SCALE(2.0) 0.8",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.5"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_pos$new("init", "0.7"),
              elem_whitespace(" "),
              option_flag$new("fixed", "fix"),
              elem_whitespace(" "),
              option_flag$new("standard", "SD")
            )
          ),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(2.0)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.8"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA SCALE  ( 0.7  ) BLOCK(4) FIX VALUES(0.5,0.01)",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "( 0.7  )", sep = "  "),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_flag$new("values", "VALUES"),
              elem_paren_open(),
              option_pos$new("diag", "0.5"),
              elem_comma(),
              option_pos$new("odiag", "0.01"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA SD SCALE(0.7) FIX BLOCK(4) VALUES(0.5,0.01)",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_flag$new("standard", "SD"),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(0.7)", sep = ""),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_flag$new("values", "VALUES"),
              elem_paren_open(),
              option_pos$new("diag", "0.5"),
              elem_comma(),
              option_pos$new("odiag", "0.01"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(4) SCALE",
        " (0.7) FIX VALUES(0.5,0.01)"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(0.7)", sep = "\n "),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(
              option_flag$new("values", "VALUES"),
              elem_paren_open(),
              option_pos$new("diag", "0.5"),
              elem_comma(),
              option_pos$new("odiag", "0.01"),
              elem_paren_close()
            )
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA block(2) scale ; (2)",
        "(3) 4 5 6"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "block", "(2)", sep = ""),
          elem_whitespace(" "),
          option_value$new("scale", "scale", "(3)", sep = " ; (2)\n"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "4"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "5"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "6"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 0.1 scale= scale(3) 0.2",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_whitespace(" "),
          option_pos$new("label", value = "scale="),
          elem_whitespace(" "),
          option_value$new("scale", "scale", "(3)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.2"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$OMEGA 0.1 scale(3) scale= 0.2",
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_whitespace(" "),
          option_value$new("scale", "scale", "(3)", sep = ""),
          elem_whitespace(" "),
          option_pos$new("label", value = "scale="),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.2"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA SCALE (0.7 ) BLOCK(4) FIX",
        "0.5",
        "0.01 0.5",
        "SCALE(2.5)",
        "0.02 0.02 SCALE(3.5) 0.003"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(0.7 )", sep = " "),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(4)", sep = ""),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.5"))
          ),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.01"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.5"))
          ),
          elem_linebreak(),
          option_value$new("scale", "SCALE", "(2.5)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.02"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.02"))
          ),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(3.5)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.003"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(2) SCALE(2)",
        "0.1",
        "scale= scale(3) 0.02 0.3"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(2)", sep = ""),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(2)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_pos$new("label", value = "scale="),
          elem_whitespace(" "),
          option_value$new("scale", "scale", "(3)", sep = ""),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.02"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.3"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(2) SCALE(2)",
        "0.1",
        "scale(3) scale= 0.02 0.3"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(2)", sep = ""),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(2)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_value$new("scale", "scale", "(3)", sep = ""),
          elem_whitespace(" "),
          option_pos$new("label", value = "scale="),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.02"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.3"))
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$OMEGA BLOCK(2) SCALE(2)",
        "0.1",
        "scale(3) FIX alabel= SD 0.02 0.3"
      ),
      want = list(
        values = list(
          option_record_name$new("omega", "OMEGA"),
          elem_whitespace(" "),
          option_value$new("block", "BLOCK", "(2)", sep = ""),
          elem_whitespace(" "),
          option_value$new("scale", "SCALE", "(2)", sep = ""),
          elem_linebreak(),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.1"))
          ),
          elem_linebreak(),
          option_value$new("scale", "scale", "(3)", sep = ""),
          elem_whitespace(" "),
          option_flag$new("fixed", "FIX"),
          elem_whitespace(" "),
          option_pos$new("label", value = "alabel="),
          elem_whitespace(" "),
          option_flag$new("standard", "SD"),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.02"))
          ),
          elem_whitespace(" "),
          option_nested$new(
            "omega",
            values = list(option_pos$new("init", "0.3"))
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case$input)
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

test_that("omega records are combined", {
  lines <- c(
    "$prob",
    "$omega 1",
    "$data foo.csv",
    "$omega block(2)",
    "0.1",
    "0.01 0.2"
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
      option_record_name$new("omega", "omega"),
      elem_whitespace(" "),
      option_nested$new(
        "omega",
        values = list(option_pos$new("init", "1"))
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      option_record_name$new("omega", "omega"),
      elem_whitespace(" "),
      option_value$new("block", "block", "(2)", sep = ""),
      elem_linebreak(),
      option_nested$new(
        "omega",
        values = list(option_pos$new("init", "0.1"))
      ),
      elem_linebreak(),
      option_nested$new(
        "omega",
        values = list(option_pos$new("init", "0.01"))
      ),
      elem_whitespace(" "),
      option_nested$new(
        "omega",
        values = list(option_pos$new("init", "0.2"))
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    format(ctl),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})

test_that("parse_omega_record() aborts on invalid vpair", {
  cases <- c(
    "$OMEGA BLOCK(3) VALUES",
    "$OMEGA BLOCK(3) VALUES(1)",
    "$OMEGA BLOCK(3) VALUES(1,2"
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})

test_that("parse_omega_record() aborts on missing or incomplete scale value", {
  cases <- list(
    # no value
    "$OMEGA SCALE 0.5 0.7",
    "$OMEGA 0.5 SCALE 0.7",
    "$OMEGA 0.5 0.7 SCALE",
    # no closing paren
    "$OMEGA SCALE(2.0 0.5 0.7",
    "$OMEGA 0.5 SCALE(2.0 0.7",
    "$OMEGA 0.5 0.7 SCALE(2.0"
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})

test_that("parse_omega_record() aborts if scale is inside init parens", {
  cases <- list(
    # NM-TRAN doesn't support these ("too many nested parentheses").
    "$OMEGA 1 (fix 2 scale(0.2))",
    "$OMEGA 1 (2 scale(0.2))"
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})
