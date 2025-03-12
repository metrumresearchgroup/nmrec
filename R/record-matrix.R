# Common options for OMEGA and SIGMA records

process_matrix_options <- function(rp, fail_on_unknown = TRUE) {
  process_options(
    rp, matrix_option_types, matrix_option_names,
    fail_on_unknown = fail_on_unknown
  )
}

matrix_option_types <- list(
  "cholesky" = option_type_flag,
  "correlation" = option_type_flag,
  "covariance" = option_type_flag,
  "fixed" = option_type_flag,
  "names" = option_type_value,
  "standard" = option_type_flag,
  "unint" = option_type_flag,
  "variance" = option_type_flag
)

diag_option_names <- list2env(
  list(
    "sd" = "standard",
    "sta" = "standard",
    "stan" = "standard",
    "stand" = "standard",
    "standa" = "standard",
    "standar" = "standard",
    "standard" = "standard",
    "var" = "variance",
    "vari" = "variance",
    "varia" = "variance",
    "varian" = "variance",
    "varianc" = "variance",
    "variance" = "variance"
  ),
  parent = param_option_names
)

matrix_option_names <- list2env(
  list(
    "cho" = "cholesky",
    "chol" = "cholesky",
    "chole" = "cholesky",
    "choles" = "cholesky",
    "cholesk" = "cholesky",
    "cholesky" = "cholesky",
    "cor" = "correlation",
    "corr" = "correlation",
    "corre" = "correlation",
    "correl" = "correlation",
    "correla" = "correlation",
    "correlat" = "correlation",
    "correlati" = "correlation",
    "correlatio" = "correlation",
    "correlation" = "correlation",
    "cov" = "covariance",
    "cova" = "covariance",
    "covar" = "covariance",
    "covari" = "covariance",
    "covaria" = "covariance",
    "covarian" = "covariance",
    "covarianc" = "covariance",
    "covariance" = "covariance",
    "names" = "names"
  ),
  parent = diag_option_names
)

matrix_has_scale_option <- function(record) {
  purrr::some(record[["values"]], function(v) {
    inherits(v, "nmrec_option_value") && identical(v[["name"]], "scale")
  })
}
