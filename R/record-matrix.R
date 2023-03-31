# Common options for OMEGA and SIGMA records

matrix_option_types <- list(
  "cholesky" = option_type_flag,
  "correlation" = option_type_flag,
  "covariance" = option_type_flag,
  "fixed" = option_type_flag,
  "names" = option_type_value,
  "standard" = option_type_flag,
  "unint" = option_type_flag,
  "values" = option_type_value,
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
    "names" = "names",
    "v" = "values",
    "va" = "values",
    "val" = "values",
    "valu" = "values",
    "value" = "values",
    "values" = "values"
  ),
  parent = diag_option_names
)
