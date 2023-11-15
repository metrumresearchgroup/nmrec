#' Set initial estimates in parameter records
#'
#' `set_theta()`, `set_omega()`, and `set_sigma()` enable updating THETA, OMEGA,
#' and SIGMA records by specifying a full-length parameter vector or matrix.
#' These functions handle mapping from the parameter space to the corresponding
#' option.
#'
#' ## Constraints and limitations
#'
#'  * These functions update initial estimates only if they are **explicitly**
#'    defined in the control stream.
#'
#'    For example, consider the update of `$THETA (1)x4`. That defines four
#'    initial estimates, but only the first explicitly appears. Calling
#'    `set_theta()` with a value of `c(5, 6, 7, 8)` gives a result of `(5)x4`.
#'
#'  * The caller must specify a value with a size that matches what is defined
#'    by the parameter records.
#'
#'  * Using additional parameter records for priors is not supported and will
#'    lead to a size mismatch between the parameter and its records. Instead use
#'    informative prior record names (such as THETAP and THETAPV).
#'
#' @param records An [nmrec_ctl_records] object.
#' @param values Overwrite the parameter's initial estimates in `records` with
#'   these values. The length must match the full length defined by the
#'   combination of the parameter's records.
#'
#'   For THETA, `values` must be a vector. For OMEGA and SIGMA, `values` can be
#'
#'   1) the square matrix, in which case the values in the lower triangle
#'      (including the diagonal) are used
#'
#'   2) a vector in row-major order for values in the lower triangle (including
#'      diagonal).
#'
#'   Use `NA` in `values` to skip updating the corresponding option.
#'
#' @seealso [set_record_option()] setting option by name.
#' @examples
#' ctl <- parse_ctl(c(
#'   "$PROBLEM ex",
#'   "$THETA 2",
#'   "$THETA (30)x2",
#'   "$OMEGA BLOCK(3)",
#'   "0.1",
#'   "0.01 0.1",
#'   "0.01 0.01 0.1"
#' ))
#' set_theta(ctl, c(10, 40, 20))
#' ctl
#'
#' new_omega <- matrix(
#'   c(
#'     0.5, 0.2, 0.2,
#'     0.2, NA, 0.2,
#'     0.2, 0.2, 0.7
#'   ),
#'   nrow = 3, byrow = TRUE
#' )
#' # The upper triangle doesn't come into play; let's zero it for clarity, but
#' # it would be fine to leave as is.
#' new_omega[upper.tri(new_omega)] <- 0
#' new_omega
#' set_omega(ctl, new_omega)
#' ctl
#' @name set_param

#' @rdname set_param
#' @export
set_theta <- function(records, values) {
  set_param(records, "theta", values)
  return(invisible(NULL))
}

#' @rdname set_param
#' @export
set_omega <- function(records, values) {
  set_matrix("omega", records, values)
  return(invisible(NULL))
}

#' @rdname set_param
#' @export
set_sigma <- function(records, values) {
  set_matrix("sigma", records, values)
  return(invisible(NULL))
}

set_matrix <- function(name, records, values) {
  ndim <- length(dim(values))
  if (ndim == 2) {
    if (nrow(values) != ncol(values)) {
      abort(paste(name, "must be square matrix.", nmrec_error()))
    }
    values <- matrix_ltri_to_vector(values)
  } else if (ndim != 0) {
    abort(
      sprintf("%s must have 0 or 2 dimensions, got %d.", name, ndim),
      nmrec_error()
    )
  }

  set_param(records, name, values)
}

set_param <- function(records, name, values) {
  stopifnot(inherits(records, "nmrec_ctl_records"))

  pinfo <- create_param_index(records, name)

  len_expected <- pinfo[["size"]]
  if (!identical(name, "theta")) {
    len_expected <- matrix_ltri_size(len_expected)
  }

  if (length(values) != len_expected) {
    abort(
      c(
        sprintf(
          "Expected length of `values` to be %d, got %d.",
          len_expected, length(values)
        ),
        "i" = paste0(
          "If you're using ", toupper(name), " records for priors, ",
          "please switch to informative record names ",
          "(such as THETAP and THETAPV)."
        )
      ),
      nmrec_error()
    )
  }

  is_na <- is.na(values)
  if (all(is_na)) {
    abort("Every value in `values` is NA.", nmrec_error())
  }

  idxs <- seq_along(values)
  keys <- param_format_key(as.character(idxs))
  ltri_to_opt <- pinfo[["ltri_to_opt"]]
  is_explicit <- purrr::map_lgl(keys, function(k) exists(k, ltri_to_opt))
  if (!any(is_explicit)) {
    bug(sprintf("No explicit %s values found.", name))
  }

  idxs_active <- idxs[is_explicit & !is_na]
  for (i in idxs_active) {
    key <- keys[i]
    res <- get(key, ltri_to_opt)
    param_set_value(res[["opt"]], values[i])
  }
}

param_set_value <- function(popt, value) {
  value <- sprintf("%.3G", value)
  if (inherits(popt, "nmrec_option_nested")) {
    if (!identical(popt[["name"]], "theta")) {
      bug("Only theta values should be nmrec_option_nested.")
    }
    param_add_init(popt, value)
  } else {
    popt$value <- value
  }
}

param_add_init <- function(popt, init) {
  vals <- popt$values
  up_idx <- purrr::detect_index(
    vals,
    function(v) inherits(v, "nmrec_option") && identical(v[["name"]], "up"),
    .dir = "backward"
  )
  if (identical(up_idx, -1L)) {
    bug(c("`up` should be present.", popt$format()))
  }

  comma_idx <- purrr::detect_index(
    vals[1:up_idx - 1],
    function(v) elem_is(v, "comma"),
    .dir = "backward"
  )
  if (comma_idx < 4) {
    # For the "(low,,up)" form, position of second comma should never be lower
    # than 4.
    bug(c("Second comma should be at position 4 or greater.", popt$format()))
  }

  popt$values <- append(
    vals, option_pos$new("init", init),
    after = comma_idx - 1L
  )
}
