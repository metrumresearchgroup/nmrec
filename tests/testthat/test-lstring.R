test_that("lstring$pop_until() works", {
  cases <- list(
    list(
      input = list(
        append = list(),
        pred = is.integer
      ),
      want = list(
        popped = list(),
        values = list(),
        idx = 1L,
        string = ""
      )
    ),
    list(
      input = list(
        append = list("abc ", "foo", " def"),
        pred = function(x) FALSE
      ),
      want = list(
        popped = list(),
        values = list("abc ", "foo", " def"),
        idx = 4L,
        string = "abc foo def"
      )
    ),
    list(
      input = list(
        append = list("abc ", 1L, " def"),
        pred = is.integer
      ),
      want = list(
        popped = list(" def"),
        values = list("abc ", 1L),
        idx = 3L,
        string = "abc 1"
      )
    ),
    list(
      input = list(
        append = list("abc ", "foo", " def", "bar"),
        pred = function(x) identical(x, "foo")
      ),
      want = list(
        popped = list(" def", "bar"),
        values = list("abc ", "foo"),
        idx = 3L,
        string = "abc foo"
      )
    )
  )

  for (case in cases) {
    lstr <- lstring$new(10L)
    for (v in case$input$append) {
      lstr$append(v)
    }

    res <- lstr$pop_until(case$input$pred)

    expect_identical(res, case$want$popped)
    expect_identical(lstr$get_values(), case$want$values)
    expect_identical(lstr$idx, case$want$idx)
    expect_identical(format(lstr), case$want$string)
  }
})

test_that("lstring grows by doubling if needed", {
  lstr <- lstring$new(4L)

  lstr$append("1")
  expect_length(lstr$values, 4)
  expect_identical(format(lstr), "1")

  lstr$append("2")
  expect_length(lstr$values, 4)
  expect_identical(format(lstr), "12")

  lstr$append("3")
  expect_length(lstr$values, 4)
  expect_identical(format(lstr), "123")

  lstr$append("4")
  expect_length(lstr$values, 4)
  expect_identical(format(lstr), "1234")

  lstr$append("5")
  expect_length(lstr$values, 8)
  expect_identical(format(lstr), "12345")
})
