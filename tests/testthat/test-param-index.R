ltv_list <- function(env) {
  l <- as.list(env)
  l[order(as.integer(sub("^p", "", names(l))))]
}

drop_popts <- function(l) {
  purrr::map(l, function(x) x[names(x) != "popts"])
}

test_that("create_param_index() works: vector", {
  cases <- list(
    list(
      lines = "$THETA 1",
      want = list(
        details = list(
          list(
            type = NA_character_,
            subtype = NA_character_,
            size = 1L
          )
        ),
        size = 1L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$THETA 1",
        "$table TIME",
        "$THETA 2 3"
      ),
      want = list(
        details = list(
          list(
            type = NA_character_,
            subtype = NA_character_,
            size = 1L
          ),
          list(
            type = NA_character_,
            subtype = NA_character_,
            size = 2L
          )
        ),
        size = 3L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 1L,
            record_index = 2L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 2L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = "$THETA (1,,2) (3,4)",
      want = list(
        details = list(
          list(
            type = NA_character_,
            subtype = NA_character_,
            size = 2L
          )
        ),
        size = 2L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_nested$new(
              "theta",
              values = list(
                elem_paren_open(),
                option_pos$new("low", "1"),
                elem_comma(),
                elem_comma(),
                option_pos$new("up", "2"),
                elem_paren_close()
              )
            ),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "4"),
            popt_index = 2L,
            record_index = 1L
          )
        )
      )
    )
  )

  for (case in cases) {
    recs <- parse_ctl(c("$prob p", case$lines))
    res <- create_param_index(recs, "theta")
    expect_identical(res$name, "theta")
    expect_identical(res$size, case$want$size)
    expect_identical(drop_popts(res$details), case$want$details)
    expect_identical(ltv_list(res$ltri_to_opt), case$want$ltri_to_opt)
  }
})

test_that("create_param_index() works: matrix", {
  cases <- list(
    list(
      lines = "$OMEGA 1",
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 1L
          )
        ),
        size = 1L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA 1 2",
        "$table TIME",
        "$OMEGA DIAG(2) 3 4"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 2L
          ),
          list(
            type = "diagonal",
            subtype = "plain",
            size = 2L
          )
        ),
        size = 4L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p6 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 1L,
            record_index = 2L
          ),
          p10 = list(
            opt = option_pos$new("init", "4"),
            popt_index = 2L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA 1",
        "$table TIME",
        "$OMEGA BLOCK(2)",
        "2",
        "3 4"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 1L
          ),
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          )
        ),
        size = 3L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 1L,
            record_index = 2L
          ),
          p5 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 2L,
            record_index = 2L
          ),
          p6 = list(
            opt = option_pos$new("init", "4"),
            popt_index = 3L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA (1)x3",
        "$table TIME",
        "$OMEGA BLOCK(2)",
        "2",
        "(3)x2"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 3L
          ),
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          )
        ),
        size = 5L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p10 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 1L,
            record_index = 2L
          ),
          p14 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 2L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA (1)x2",
        "$table TIME",
        "$OMEGA BLOCK(3)",
        "(2 3)x3"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 2L
          ),
          list(
            type = "block",
            subtype = "plain",
            size = 3L
          )
        ),
        size = 5L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p6 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 1L,
            record_index = 2L
          ),
          p9 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 1L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA 1",
        "$table TIME",
        "$OMEGA BLOCK(2) VALUES(2,3)"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 1L
          ),
          list(
            type = "block",
            subtype = "vpair",
            size = 2L
          )
        ),
        size = 3L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("diag", "2"),
            popt_index = 1L,
            record_index = 2L
          ),
          p5 = list(
            opt = option_pos$new("odiag", "3"),
            popt_index = 1L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(2)",
        "1",
        "2 3",
        "$table TIME",
        "$OMEGA BLOCK(2) SAME"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          ),
          list(
            type = "block",
            subtype = "same",
            size = 2L
          )
        ),
        size = 4L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 3L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(2)",
        "1",
        "2 3",
        "$table TIME",
        "$OMEGA BLOCK SAME"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          ),
          list(
            type = "block",
            subtype = "same",
            size = 2L
          )
        ),
        size = 4L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 3L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(4)",
        "$OMEGA DIAGONAL(2) 1 2"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "no_inits",
            size = 4L
          ),
          list(
            type = "diagonal",
            subtype = "plain",
            size = 2L
          )
        ),
        size = 6L,
        ltri_to_opt = list(
          p15 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 2L
          ),
          p21 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 2L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA DIAGONAL(3)"
      ),
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "no_inits",
            size = 3L
          )
        ),
        size = 3L,
        ltri_to_opt = list()
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(2)",
        "1",
        "2 3",
        "$table TIME",
        "$OMEGA BLOCK SAME(3)"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          ),
          list(
            type = "block",
            subtype = "same",
            size = 6L
          )
        ),
        size = 8L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 3L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(2)",
        "1",
        "2 3",
        "$table TIME",
        "$OMEGA BLOCK SAME"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          ),
          list(
            type = "block",
            subtype = "same",
            size = 2L
          )
        ),
        size = 4L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 3L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = c(
        "$OMEGA SCALE(1.5) BLOCK(2)",
        "1",
        "2 SCALE(2.5) 3"
      ),
      want = list(
        details = list(
          list(
            type = "block",
            subtype = "plain",
            size = 2L
          )
        ),
        size = 2L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p2 = list(
            opt = option_pos$new("init", "2"),
            popt_index = 2L,
            record_index = 1L
          ),
          p3 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 3L,
            record_index = 1L
          )
        )
      )
    ),
    list(
      lines = "$OMEGA (1)x2 SCALE (0.5) 3",
      want = list(
        details = list(
          list(
            type = "diagonal",
            subtype = "implicit",
            size = 3L
          )
        ),
        size = 3L,
        ltri_to_opt = list(
          p1 = list(
            opt = option_pos$new("init", "1"),
            popt_index = 1L,
            record_index = 1L
          ),
          p6 = list(
            opt = option_pos$new("init", "3"),
            popt_index = 2L,
            record_index = 1L
          )
        )
      )
    )
  )

  for (case in cases) {
    recs <- parse_ctl(c("$prob p", case$lines))
    res <- create_param_index(recs, "omega")
    expect_identical(res$name, "omega")
    expect_identical(res$size, case$want$size)
    expect_identical(drop_popts(res$details), case$want$details)
    expect_identical(ltv_list(res$ltri_to_opt), case$want$ltri_to_opt)
  }
})

test_that("create_param_index() aborts: no initial estimates", {
  cases <- list(
    list(line = "$THETA", name = "theta"),
    list(line = "$OMEGA", name = "omega")
  )
  for (case in cases) {
    recs <- parse_ctl(c("$prob prob", case$line))
    expect_error(
      create_param_index(recs, case$name),
      "no initial estimates"
    )
  }
})

test_that("create_param_index() aborts: size mismatch", {
  cases <- c(
    "$OMEGA DIAG(2) 1 2 3",
    "$OMEGA BLOCK(1) 1 2",
    "$OMEGA BLOCK(2) 1 2",
    "$OMEGA BLOCK(2) 1 2 3 4"
  )
  for (case in cases) {
    recs <- parse_ctl(c("$prob prob", case))
    expect_error(
      create_param_index(recs, "omega"),
      "Expected .* initial estimates"
    )
  }
})

test_that("create_param_index() aborts: bad (N)", {
  cases <- c(
    "$OMEGA DIAG(X)",
    "$OMEGA BLOCK(X)",
    "$OMEGA BLOCK(2) SAME(X)"
  )
  for (case in cases) {
    recs <- parse_ctl(c("$prob prob", case))
    expect_error(
      create_param_index(recs, "omega"),
      "Failed to parse"
    )
  }
})

test_that("create_param_index() aborts: invalid block/same", {
  expect_error(
    create_param_index(
      parse_ctl(c("$prob prob", "$OMEGA BLOCK")),
      "omega"
    ),
    "must define (N)",
    fixed = TRUE
  )
  expect_error(
    create_param_index(
      parse_ctl(c("$prob prob", "$OMEGA BLOCK SAME")),
      "omega"
    ),
    "SAME cannot be used"
  )
  expect_error(
    create_param_index(
      parse_ctl(c("$prob prob", "$OMEGA DIAG(2)", "$OMEGA BLOCK SAME")),
      "omega"
    ),
    "SAME cannot be used"
  )
  expect_error(
    create_param_index(
      parse_ctl(c("$prob prob", "$OMEGA BLOCK(2)", "$OMEGA BLOCK SAME 1")),
      "omega"
    ),
    "SAME cannot include explicit"
  )
})

test_that("create_param_index() aborts: unrecognized name", {
  expect_error(
    create_param_index(
      parse_ctl(c("$prob prob", "$OMEGA BLOCK")),
      "FOO"
    ),
    "FOO"
  )
})
