# Reduced version of `deparse1()` isn't available until R 4.0.
deparse_string <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}
