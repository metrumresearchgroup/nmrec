test_that("matrix_ltri_to_vector() works", {
  cases <- list(
    list(input = 1, want = 1),
    list(
      input = matrix(1, nrow = 1, ncol = 1),
      want = 1
    ),
    list(
      input = matrix(as.numeric(1:4), nrow = 2, ncol = 2),
      want = c(1, 2, 4)
    ),
    list(
      input = matrix(as.numeric(1:9), nrow = 3, ncol = 3),
      want = c(1, 2, 5, 3, 6, 9)
    )
  )
  for (case in cases) {
    expect_identical(matrix_ltri_to_vector(!!case$input), case$want)
  }
})

test_that("matrix_ltri_size() works", {
  cases <- list(
    list(input = 1, want = 1L),
    list(input = 2, want = 3L),
    list(input = 3, want = 6L),
    list(input = 4, want = 10L)
  )
  for (case in cases) {
    expect_identical(matrix_ltri_size(!!case$input), case$want)
  }
})

test_that("matrix_ltri_indices() works", {
  cases <- list(
    list(input = 1, want = as.matrix(1L)),
    list(
      input = 2,
      want = matrix(
        # fmt: skip
        c(
          1L, NA_integer_,
          2L, 3L
        ),
        nrow = 2,
        byrow = TRUE
      )
    ),
    list(
      input = 5,
      want = matrix(
        # fmt: skip
        c(
          1L, NA_integer_, NA_integer_, NA_integer_, NA_integer_,
          2L, 3L, NA_integer_, NA_integer_, NA_integer_,
          4L, 5L, 6L, NA_integer_, NA_integer_,
          7L, 8L, 9L, 10L, NA_integer_,
          11L, 12L, 13L, 14L, 15L
        ),
        nrow = 5,
        byrow = TRUE
      )
    )
  )
  for (case in cases) {
    expect_identical(matrix_ltri_indices(!!case$input), case$want)
  }
})

test_that("vector_to_matrix_ltri() works", {
  cases <- list(
    list(
      input = 1,
      n = 1,
      want = matrix(1, nrow = 1, ncol = 1)
    ),
    list(
      input = 1,
      n = 1,
      want = matrix(1, nrow = 1, ncol = 1)
    ),
    list(
      input = c(1, 2, 4),
      n = 2,
      want = matrix(
        # fmt: skip
        c(
          1, NA_real_,
          2, 4
        ),
        nrow = 2,
        ncol = 2,
        byrow = TRUE
      )
    ),
    list(
      input = c(1, 2, 5, 3, 6, 9),
      n = 3,
      want = matrix(
        # fmt: skip
        c(
          1, NA_real_, NA_real_,
          2, 5, NA_real_,
          3, 6, 9
        ),
        nrow = 3,
        ncol = 3,
        byrow = TRUE
      )
    )
  )
  for (case in cases) {
    expect_identical(
      vector_to_matrix_ltri(!!case$input, !!case$n),
      case$want
    )
  }
})

test_that("vector_to_matrix_ltri() aborts on size mismatch", {
  cases <- list(
    list(
      input = 1,
      n = 2
    ),
    list(
      input = c(1, 1),
      n = 2
    ),
    list(
      input = c(1, 1, 1),
      n = 3
    )
  )
  for (case in cases) {
    expect_error(
      vector_to_matrix_ltri(!!case$input, !!case$n),
      class = "nmrec_error"
    )
  }
})

test_that("matrix -> vector -> matrix roundtrip", {
  cases <- list(
    list(input = 1, n = 1),
    list(input = matrix(1, nrow = 1, ncol = 1), n = 1),
    list(input = matrix(as.numeric(1:4), nrow = 2, ncol = 2), n = 2),
    list(input = matrix(as.numeric(1:9), nrow = 3, ncol = 3), n = 3)
  )
  for (case in cases) {
    mat <- as.matrix(case$input)
    mat[upper.tri(mat, diag = FALSE)] <- NA
    expect_identical(
      vector_to_matrix_ltri(
        matrix_ltri_to_vector(case$input),
        n = case$n
      ),
      mat
    )
  }
})

test_that("vector -> matrix -> vector roundtrip", {
  cases <- list(
    list(input = 1, n = 1),
    list(input = c(1, 2, 4), n = 2),
    list(input = c(1, 20, 30, 400, 500, 600), n = 3)
  )
  for (case in cases) {
    expect_identical(
      matrix_ltri_to_vector(
        vector_to_matrix_ltri(case$input, n = case$n)
      ),
      case$input
    )
  }
})

test_that("matrix_sub_diag() works", {
  expect_identical(
    matrix_sub_diag(as.matrix(1), 1, 1),
    1
  )

  m <- matrix(1:49, nrow = 7, byrow = TRUE)
  expect_identical(
    matrix_sub_diag(m, 1, 1),
    1L
  )
  expect_identical(
    matrix_sub_diag(m, 1, 2),
    c(1L, 9L)
  )
  expect_identical(
    matrix_sub_diag(m, 2, 1),
    9L
  )
  expect_identical(
    matrix_sub_diag(m, 2, 2),
    c(9L, 17L)
  )
  expect_identical(
    matrix_sub_diag(m, 1, 7),
    c(1L, 9L, 17L, 25L, 33L, 41L, 49L)
  )
  expect_identical(
    matrix_sub_diag(m, 5, 2),
    c(33L, 41L)
  )
  expect_identical(
    matrix_sub_diag(m, 7, 1),
    49L
  )
})

test_that("matrix_sub_ltri() works", {
  expect_identical(
    matrix_sub_ltri(as.matrix(1), 1, 1),
    1
  )

  m <- matrix(1:49, nrow = 7, byrow = TRUE)
  expect_identical(
    matrix_sub_ltri(m, 1, 1),
    1L
  )
  expect_identical(
    matrix_sub_ltri(m, 1, 2),
    c(1L, 8L, 9L)
  )
  expect_identical(
    matrix_sub_ltri(m, 2, 1),
    9L
  )
  expect_identical(
    matrix_sub_ltri(m, 2, 2),
    c(9L, 16L, 17L)
  )
  expect_identical(
    matrix_sub_ltri(m, 1, 7),
    # fmt: skip
    c(
      1L,
      8L, 9L,
      15L, 16L, 17L,
      22L, 23L, 24L, 25L,
      29L, 30L, 31L, 32L, 33L,
      36L, 37L, 38L, 39L, 40L, 41L,
      43L, 44L, 45L, 46L, 47L, 48L, 49L
    )
  )
  expect_identical(
    matrix_sub_ltri(m, 5, 2),
    c(33L, 40L, 41L)
  )
  expect_identical(
    matrix_sub_ltri(m, 7, 1),
    49L
  )
})
