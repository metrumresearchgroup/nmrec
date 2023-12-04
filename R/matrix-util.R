#' Return lower triangle and diagonal of a square matrix as vector
#'
#' The returned values are in row-major order.
#'
#'     1  .  .
#'     2  3  .  =>  1 2 3 4 5 6
#'     4  5  6
#'
#' @noRd
matrix_ltri_to_vector <- function(x) {
  x <- t(x) # Transpose for row-major order.
  return(as.vector(x[upper.tri(x, diag = TRUE)]))
}

#' Return size of the lower triangle and diagonal of an NxN matrix
#'
#' @noRd
matrix_ltri_size <- function(n) {
  as.integer(n * (n + 1L) / 2L)
}

#' Return matrix indices for lower triangle and diagonal of an NxN matrix
#'
#' Increment the index values in row-major order, and set values outside the
#' lower triangle and diagonal to `NA`.
#'
#'                 1  NA  NA
#'     n=3   =>    2   3  NA
#'                 4   5   6
#'
#' @noRd
matrix_ltri_indices <- function(n) {
  x <- matrix(NA_integer_, nrow = n, ncol = n)
  x[upper.tri(x, diag = TRUE)] <- seq_len(matrix_ltri_size(n))
  return(t(x))
}

#' Subset diagonal of a square matrix
#'
#' @param x A square matrix.
#' @param start Start at this row and column.
#' @param size Number of diagonal values to include.
#'
#'     1   .   .   .   start=2
#'     2   3   .   .    size=2   =>  3 6
#'     4   5   6   .
#'     7   8   9  10
#'
#' @noRd
matrix_sub_diag <- function(x, start, size) {
  to <- start + size - 1L
  if (start == to) {
    return(x[start, start])
  }
  return(diag(x[start:to, start:to]))
}

#' Subset lower triangle and diagonal of a square matrix
#'
#' @param x A square matrix.
#' @param start Start at this row and column.
#' @param size Number of _diagonal_ values to include.
#'
#'     1   .   .   .   start=2
#'     2   3   .   .    size=2   =>  3 5 6
#'     4   5   6   .
#'     7   8   9  10
#'
#' @noRd
matrix_sub_ltri <- function(x, start, size) {
  to <- start + size - 1L
  if (start == to) {
    return(x[start, start])
  }

  s <- t(x[start:to, start:to]) # Transpose for row-major order.
  return(s[upper.tri(s, diag = TRUE)])
}
