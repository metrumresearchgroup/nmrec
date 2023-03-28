test_that("tstring$pop_until() works", {
  cases <- list(
    list(
      input = list(
        append = list(),
        pop = list(is.integer)
      ),
      want = list(
        pop = list(),
        template = list(),
        idx_t = 1L,
        string = ""
      )
    ),
    list(
      input = list(
        append = list(
          t = list("abc "),
          v = list("foo", "foo"),
          t = list(" def")
        ),
        pop = list(function(x) FALSE)
      ),
      want = list(
        pop = list(),
        template = list("abc ", 1L, " def"),
        idx_t = 4L,
        string = "abc foo def"
      )
    ),
    list(
      input = list(
        append = list(
          t = list("abc "),
          v = list("foo", "foo"),
          t = list(" def")
        ),
        pop = list(is.integer)
      ),
      want = list(
        pop = list(" def"),
        template = list("abc ", 1L),
        idx_t = 3L,
        string = "abc foo"
      )
    ),
    list(
      input = list(
        append = list(
          t = list("abc "),
          v = list("foo", "foo"),
          t = list(" def"),
          v = list("bar", "bar")
        ),
        pop = list(function(x) identical(x, 1L))
      ),
      want = list(
        pop = list(" def", 2L),
        template = list("abc ", 1L),
        idx_t = 3L,
        string = "abc foo"
      )
    )
  )

  for (case in cases) {
    templ <- tstring$new(10L, 10L)

    purrr::iwalk(case$input$append, function(x, kind) {
      fn <- switch(kind,
        t = "append_t",
        v = "append_v",
        abort("name should be t or v")
      )
      do.call(templ[[fn]], x)
    })

    res <- templ$pop_until(case$input$pop)

    expect_identical(res, case$want$pop)
    expect_identical(templ$get_template(), case$want$template)
    expect_identical(templ$idx_t, case$want$idx_t)
    expect_identical(format(templ), case$want$string)
  }
})
