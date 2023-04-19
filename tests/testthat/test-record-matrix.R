ls_all <- function(e) {
  fn <- function(e, names) {
    if (identical(e, emptyenv())) {
      return(names)
    }
    fn(parent.env(e), c(names, ls(e)))
  }

  fn(e, character(0))
}

test_that("matrix_option_types and matrix_option_names align", {
  expect_setequal(
    names(matrix_option_types),
    purrr::map_chr(
      ls_all(matrix_option_names),
      function(x) get(x, envir = matrix_option_names)
    )
  )
})

test_that("matrix_option_names doesn't repeat parent values", {
  expect_length(
    intersect(ls(diag_option_names), ls(matrix_option_names)),
    0
  )
  expect_length(
    intersect(ls(param_option_names), ls(diag_option_names)),
    0
  )
  expect_length(
    intersect(ls(param_option_names), ls(matrix_option_names)),
    0
  )
})
