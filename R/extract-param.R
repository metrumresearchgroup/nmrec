#' Extract initial estimates from parameter records
#'
#' @description
#'
#' Construct a numeric object from initial estimates defined by control stream
#' records:
#'
#'  * `extract_theta()` returns a vector of initial estimates defined by `THETA`
#'     records.
#'
#'  * `extract_omega()` and `extract_sigma()` return a matrix of initial
#'     estimates for `OMEGA` and `SIGMA` records.
#'
#' @details
#'
#' ## Constraints and limitations
#'
#'  * These functions return initial estimates only if they are **explicitly**
#'    defined in the control stream. All other values are returned as `NA`. For
#'    example, a record of `$THETA (1)x4` is returned as `c(1, NA, NA, NA)`.
#'
#'  * If additional parameter records are used for priors, those are included in
#'    the result. To avoid this, instead use informative prior record names
#'    (such as `THETAP` and `THETAPV`).
#'
#' @param records An [nmrec_ctl_records] object.
#' @return A vector (for `THETA`) or a square matrix (for `OMEGA` and `SIGMA`).
#'   For matrix values, the upper triangle is always filled with `NA` values.
#' @seealso [set_param] for setting parameter options from values
#' @examples
#' ctl <- parse_ctl(c(
#'   "$PROBLEM ex",
#'   "$THETA 2 FIX",
#'   "$THETA (10,30)x2",
#'   "$OMEGA BLOCK(3)",
#'   "0.1",
#'   "0.01 0.1",
#'   "0.01 0.01 0.1"
#' ))
#' extract_theta(ctl)
#'
#' extract_omega(ctl)
#' @name extract_param

#' @rdname extract_param
#' @export
extract_theta <- function(records) {
  pinfo <- create_param_index(records, "theta")
  fn <- function(key) param_get_init(pinfo, key)

  size <- pinfo[["size"]]
  res <- param_fill(rep(NA_real_, size), fn)

  return(res)
}

#' @rdname extract_param
#' @export
extract_omega <- function(records) {
  return(extract_matrix("omega", records))
}

#' @rdname extract_param
#' @export
extract_sigma <- function(records) {
  return(extract_matrix("sigma", records))
}

extract_matrix <- function(name, records) {
  pinfo <- create_param_index(records, name)
  size <- pinfo[["size"]]
  lsize <- matrix_ltri_size(size)

  res <- param_fill(
    rep(NA_real_, lsize),
    function(key) param_get_init(pinfo, key)
  )
  res <- vector_to_matrix_ltri(res, size)

  return(res)
}

#' Call `fn` with an index-based key ("pN") and update the corresponding
#' position in `values` with the result.
#' @noRd
param_fill <- function(values, fn) {
  keys <- param_format_key(as.character(seq_along(values)))
  for (idx in seq_along(values)) {
    key <- keys[idx]
    res <- fn(key)
    if (is.null(res)) {
      next
    }
    values[idx] <- res
  }

  return(values)
}

### Accessor functions for `param_fill`

param_get_init <- function(info, key) {
  ltri_to_opt <- info[["ltri_to_opt"]]
  opt <- ltri_to_opt[[key]][["opt"]]
  if (!is.null(opt) && opt$name %in% c("init", "diag", "odiag")) {
    return(as.numeric(opt$value))
  }
}

param_has_flag <- function(info, key, flag) {
  opt_info <- info[["ltri_to_opt"]][[key]]
  if (!is.null(opt_info)) {
    # Consider flags at the record level (e.g., "$OMEGA BLOCK(2) FIX ...")...
    ridx <- opt_info[["record_index"]]
    rec <- info[["records"]][[ridx]]
    opt <- get_record_option_impl(rec$values, flag)
    if (is.null(opt)) {
      # ... or the parameter option level (e.g., "$OMEGA DIAG(2) 1 2 FIX").
      pidx <- opt_info[["popt_index"]]
      popt <- info[["details"]][[ridx]][["popts"]][[pidx]]
      opt <- get_record_option_impl(popt$values, flag)
    }

    return(!(is.null(opt) || is.null(opt$value)))
  }
}
