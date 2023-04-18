test_that("option$name is read-only", {
  opt <- option$new("foo")
  expect_identical(opt$name, "foo")
  expect_error(opt$name <- "x", class = "nmrec_error")
})

test_that("options with NULL values format to empty strings", {
  cases <- list(
    list(
      opt = option_pos,
      args = list("foo", value = NULL)
    ),
    list(
      opt = option_flag,
      args = list("foo", "foo", value = NULL)
    ),
    list(
      opt = option_value,
      args = list("foo", "foo", value = NULL)
    ),
    list(
      opt = option_nested,
      args = list("foo", values = NULL)
    )
  )

  for (case in cases) {
    expect_identical(format(do.call(case$opt$new, args = case$args)), "")
  }
})
