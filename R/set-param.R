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
#' @param fmt Convert each value to a string with this [sprintf()] format
#'   specifier.
#' @param representation Whether to keep alternative representation options like
#'   SD and CORRELATION or reset to the default representation (variance and
#'   covariance).
#'
#'   For OMEGA and SIGMA records, NONMEM supports specifying diagonal and
#'   off-diagonal initial estimates in a different representation than the
#'   default, variance and covariance. If `values` are the final estimates from
#'   a previous NONMEM run, the alternative representation options should be
#'   discarded because NONMEM always outputs variances and covariances.
#'
#'   `NA` values can lead to records and options being "untouched" (i.e. not
#'   overwritten with the value from `values`). Even when "reset" is
#'   specified, alternative options are not discarded for untouched _records_
#'   (in the case of BLOCK records, where the option applies to all values in
#'   the block) or for untouched _options_ (in the case of DIAGONAL records,
#'   where the options apply to individual initial estimates).
#' @param bounds Whether to keep or discard the existing bounds when setting the
#'   initial estimates in THETA records.
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
set_theta <- function(records, values, fmt = "%.3G",
                      bounds = c("keep", "discard")) {
  bounds <- rlang::arg_match(bounds)

  res <- set_param(records, "theta", values, fmt)
  if (identical(bounds, "discard")) {
    details <- res[["pinfo"]][["details"]]
    modified_records <- res[["modified_records"]]
    modified_popts <- res[["modified_popts"]]
    ridxs <- unique(modified_records[modified_records != 0])
    for (ridx in ridxs) {
      popts <- details[[ridx]][["popts"]]
      oidxs <- unique(modified_popts[modified_records == ridx])
      purrr::walk(popts[oidxs], theta_discard_bounds)
    }
  }
  return(invisible(NULL))
}

#' @rdname set_param
#' @export
set_omega <- function(records, values, fmt = "%.3G",
                      representation = c("keep", "reset")) {
  representation <- rlang::arg_match(representation)
  set_matrix("omega", records, values, fmt, representation)
  return(invisible(NULL))
}

#' @rdname set_param
#' @export
set_sigma <- function(records, values, fmt = "%.3G",
                      representation = c("keep", "reset")) {
  representation <- rlang::arg_match(representation)
  set_matrix("sigma", records, values, fmt, representation)
  return(invisible(NULL))
}

set_matrix <- function(name, records, values, fmt, representation) {
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

  res <- set_param(records, name, values, fmt)
  if (identical(representation, "reset")) {
    pinfo <- res[["pinfo"]]
    details <- pinfo[["details"]]
    modified_records <- res[["modified_records"]]
    modified_popts <- res[["modified_popts"]]
    ridxs <- unique(modified_records[modified_records != 0])
    for (ridx in ridxs) {
      if (identical(details[[ridx]][["type"]], "diagonal")) {
        # For diagonal, options like SD are attached to individual estimates.
        popts <- details[[ridx]][["popts"]]
        oidxs <- unique(modified_popts[modified_records == ridx])
        purrr::walk(popts[oidxs], matrix_reset_var_covar)
      } else {
        # For block, options like SD apply to all values.
        matrix_reset_var_covar(pinfo[["records"]][[ridx]])
      }
    }
  }
}

#' Set initial estimates in the specified parameter records
#'
#' To support further actions on what was set, return a list with three items:
#'
#'   * pinfo: index information returned by `create_param_index()`
#'
#'   * modified_records: for each "set" call, the index the corresponding record
#'     in pinfo
#'
#'   * modified_popts: for each "set" call, index of the corresponding popt in
#'     the pinfo's popts list for a given record. (This value is only unique
#'     when considered together with the modified_records value.)
#'
#' @noRd
set_param <- function(records, name, values, fmt) {
  stopifnot(inherits(records, "nmrec_ctl_records"))
  stopifnot(length(fmt) == 1, is.character(fmt), nzchar(fmt))

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
  record_idxs <- integer(length(idxs_active))
  popt_idxs <- integer(length(idxs_active))
  for (i in idxs_active) {
    key <- keys[i]
    res <- get(key, ltri_to_opt)
    param_set_value(res[["opt"]], values[i], fmt)
    record_idxs[i] <- res[["record_index"]]
    popt_idxs[i] <- res[["popt_index"]]
  }

  return(list(
    pinfo = pinfo,
    modified_records = record_idxs,
    modified_popts = popt_idxs
  ))
}

param_set_value <- function(popt, value, fmt) {
  value <- sprintf(fmt, value)
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

theta_discard_bounds <- function(popt) {
  vals <- popt$values

  if (is.null(get_record_option_impl(vals, "init"))) {
    bug(c("Option should have init at this point.", popt$format()))
  }

  has_low <- !is.null(get_record_option_impl(vals, "low"))
  if (!has_low) {
    return(NULL)
  }

  has_up <- !is.null(get_record_option_impl(vals, "up"))
  drop_cw <- FALSE
  lstr <- lstring$new()
  for (v in vals) {
    if (drop_cw) {
      if (elem_is(v, c("comma", "whitespace"))) {
        next
      }
      drop_cw <- FALSE
    }

    if (!inherits(v, "nmrec_option")) {
      lstr$append(v)
      next
    }

    name <- v[["name"]]
    if (identical(name, "low")) {
      drop_cw <- TRUE
      next
    }

    if (identical(name, "init")) {
      lstr$append(v)
      drop_cw <- has_up
      next
    }

    if (identical(name, "up")) {
      next
    }
    lstr$append(v)
  }

  popt$values <- param_maybe_drop_parens(lstr$get_values())
}

#' Discard options that override the default variance/covariance representation
#'
#' @param obj An object with a "values" field that has option and element
#'   values.
#'
#' @noRd
matrix_reset_var_covar <- function(obj) {
  to_reset <- c(
    "cholesky",
    "correlation",
    "standard"
  )

  lstr <- lstring$new()
  drop_cw <- FALSE
  # Go in reverse because it makes it easier to handle the leading spaces/comma.
  for (v in rev(obj$values)) {
    if (drop_cw) {
      if (elem_is(v, c("comma", "whitespace"))) {
        next
      }
      drop_cw <- FALSE
    }
    if (inherits(v, "nmrec_option_flag") && v[["name"]] %in% to_reset) {
      drop_cw <- TRUE
      next
    }
    lstr$append(v)
  }
  obj$values <- param_maybe_drop_parens(rev(lstr$get_values()))
}

param_maybe_drop_parens <- function(values) {
  is_alone <- length(values) == 3 &&
    elem_is(values[[1]], "paren_open") &&
    elem_is(values[[3]], "paren_close")
  if (is_alone) {
    values <- values[2]
  }

  return(values)
}
