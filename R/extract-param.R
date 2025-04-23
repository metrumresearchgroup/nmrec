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
#'    the result. To avoid this, instead use more specific record names (such as
#'    `THETAP` and `THETAPV`).
#'
#' @param records An [nmrec_ctl_records] object.
#' @param mark_flags A vector of NONMEM flags (i.e. valueless options such as
#'   `FIXED` or `SD`). For each specified flag, construct a boolean vector (for
#'   `THETA`) or matrix (for `OMEGA` and `SIGMA`) indicating whether the flag is
#'   "active" for the value. Any valid spelling of the flag name is allowed.
#'
#'   For example, passing a value of "fix" would indicate whether a `FIXED` flag
#'   is in effect for each value. The flag may be linked to an individual
#'   initial estimate (e.g., `FIXED` for a `THETA` or diagonal `OMEGA`record) or
#'   linked to all values in the record (e.g., `FIXED` for a `THETA` or block
#'   `OMEGA` record). In either case, the boolean vector or matrix is always the
#'   same shape as the return value.
#'
#'   The same result will be returned regardless of `type`. For example, passing
#'   `mark_flags = "fix"` and `type = "up"` would yield `TRUE` for a `THETA`
#'   initial estimate of `(1,2 FIX)`.
#'
#'   The results are stored in a named list as the "nmrec_flags" attribute
#'   attached to the return value, with the names corresponding to the resolved
#'   flag names.
#' @param type Which `THETA` value to return: initial estimate ("init", the
#'   default), the lower bound ("low"), or the upper bound ("up").
#' @return A vector (for `THETA`) or a square matrix (for `OMEGA` and `SIGMA`).
#'   For matrix values, the upper triangle is always filled with `NA` values.
#'
#'   The "nmrec_record_size" attribute attached to the return value indicates
#'   the number of values that come from each record. For matrix results, the
#'   value corresponds to the size along the diagonal.
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
#' extract_theta(ctl, type = "low")
#' extract_theta(ctl, mark_flags = "fixed")
#'
#' extract_omega(ctl)
#' @name extract_param

#' @rdname extract_param
#' @export
extract_theta <- function(
  records,
  mark_flags = NULL,
  type = c("init", "low", "up")
) {
  type <- rlang::arg_match(type)

  flags <- NULL
  if (length(mark_flags)) {
    flags <- mark_flags_prepare(
      mark_flags,
      theta_option_names,
      theta_option_types
    )
  }

  pinfo <- create_param_index(records, "theta")
  if (identical(type, "init")) {
    fn <- function(key) param_get_init(pinfo, key)
  } else {
    fn <- function(key) param_get_bound(pinfo, key, type)
  }

  size <- pinfo[["size"]]
  res <- param_fill(rep(NA_real_, size), fn)
  attr(res, "nmrec_record_size") <- purrr::map_int(pinfo[["details"]], "size")

  if (length(flags)) {
    flags <- purrr::map(flags, function(flag) {
      param_fill(
        rep(NA, size),
        function(key) param_has_flag(pinfo, key, flag)
      )
    })
    attr(res, "nmrec_flags") <- flags
  }

  return(res)
}

#' @rdname extract_param
#' @export
extract_omega <- function(records, mark_flags = NULL) {
  return(extract_matrix("omega", records, mark_flags))
}

#' @rdname extract_param
#' @export
extract_sigma <- function(records, mark_flags = NULL) {
  return(extract_matrix("sigma", records, mark_flags))
}

extract_matrix <- function(name, records, mark_flags) {
  flags <- NULL
  if (length(mark_flags)) {
    flags <- mark_flags_prepare(
      mark_flags,
      matrix_option_names,
      matrix_option_types
    )
  }

  pinfo <- create_param_index(records, name)
  size <- pinfo[["size"]]
  lsize <- matrix_ltri_size(size)

  for (rec in pinfo[["records"]]) {
    if (matrix_has_scale_option(rec)) {
      warning(
        "SCALE option should be accounted for when working with raw values:\n",
        rec$format()
      )
    }
  }

  res <- param_fill(
    rep(NA_real_, lsize),
    function(key) param_get_init(pinfo, key)
  )
  res <- vector_to_matrix_ltri(res, size)
  attr(res, "nmrec_record_size") <- purrr::map_int(pinfo[["details"]], "size")

  if (length(flags)) {
    attr(res, "nmrec_flags") <- purrr::map(flags, function(flag) {
      vec <- param_fill(
        rep(NA, lsize),
        function(key) param_has_flag(pinfo, key, flag)
      )
      return(vector_to_matrix_ltri(vec, size))
    })
  }

  return(res)
}

mark_flags_prepare <- function(flags, option_map, type_map) {
  resolve <- function(option_map, type_map, name) {
    found <- get0(tolower(name), option_map)
    if (!(is.null(found) || identical(type_map[[found]], "flag"))) {
      abort(
        sprintf("'%s' is not a flag option.", name),
        nmrec_error()
      )
    }
    return(found)
  }

  res <- purrr::map(flags, function(x) {
    resolve(option_map, type_map, x) %||%
      param_get_value_option(x)
  })

  unknown <- purrr::map_lgl(res, is.null)
  if (any(unknown)) {
    abort(
      c(
        "`mark_flags` contains unknown flags.",
        as.character(flags[unknown])
      ),
      nmrec_error()
    )
  }

  # Assign names to get names on the purrr::map() results. It's tempting to use
  # `flags` here so that the caller gets back the names as they specified them;
  # that doesn't work well though because different records or parameter options
  # may use different names (e.g., "$THETA 1 FIX 2 fix").
  names(res) <- res
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

param_get_bound <- function(info, key, type) {
  opt_info <- info[["ltri_to_opt"]][[key]]
  if (!is.null(opt_info)) {
    ridx <- opt_info[["record_index"]]
    pidx <- opt_info[["popt_index"]]

    details <- info[["details"]]
    popt <- details[[ridx]][["popts"]][[pidx]]
    bound_opt <- get_record_option_impl(popt$values, type)
    if (!is.null(bound_opt)) {
      return(as.numeric(bound_opt$value))
    }
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
