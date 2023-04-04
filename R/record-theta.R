parse_theta_record <- function() {
  rp <- record_parser$new("theta", private$name_raw, private$lines)

  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  rp$walk(function(r) {
    process_options(
      r, theta_option_types, theta_option_names,
      fail_on_unknown = FALSE
    )

    if (!r$done()) {
      param_parse_label(r)
      r$gobble_one("whitespace")
      parse_theta_value(r)
    }
  })
  rp$assert_done()

  return(rp$get_values())
}

#' Parse single theta value
#'
#' A "value" is the stretch of everything associated with a particular theta
#' (e.g. "7", "7 FIXED", or "(0, 7) FIXED").
#'
#' @noRd
parse_theta_value <- function(rp) {
  lstr <- lstring$new()
  rp$gobble(lstr = lstr)

  if (rp$is("paren_open")) {
    parse_theta_paren(rp, lstr)
  } else {
    val <- rp$yank()
    check_for_short_unint(val)
    lstr$append(option_pos$new("init", value = val))
    rp$gobble(lstr = lstr)
    process_theta_value_option(rp, lstr)
  }

  param_append("theta", rp, lstr)
}

#' Parse a theta value starting on open paren
#'
#' @param rp `record_parser` object.
#' @param lstr `lstring` object.
#' @noRd
parse_theta_paren <- function(rp, lstr) {
  end <- rp$find_closing_paren("linebreak")
  lstr$append(rp$yank())
  rp$gobble_one("whitespace", lstr = lstr)
  valnames <- c("low", "init", "up")
  idx_val <- get_theta_value_idx(rp, end)

  while (rp$idx_e < end) {
    process_theta_value_option(rp, lstr)
    if (rp$idx_e >= end) {
      break
    }

    if (rp$is("whitespace")) {
      lstr$append(rp$yank())
      next
    }
    if (rp$is("comma")) {
      # (low,,up) form
      no_init <- identical(idx_val, 2L) &&
        rp$is("comma", pos = rp$idx_e + 1) ||
        (rp$is("whitespace", pos = rp$idx_e + 1) &&
          rp$is("whitespace", pos = rp$idx_e + 2))
      if (no_init) {
        idx_val <- idx_val + 1L
      }
      lstr$append(rp$yank())
      next
    }

    if (inherits(rp$current(), "nmrec_element")) {
      abort(
        c(
          sprintf(
            "Unexpected element (%s) with parens.",
            rp$current()
          ),
          rp$format()
        ),
        "nmrec_parse_error"
      )
    }

    if (idx_val > 3) {
      abort(
        c(
          "Bug: there should be no more than 3 values.",
          rp$format()
        ),
        "nmrec_dev_error"
      )
    }

    val <- rp$yank()
    check_for_short_unint(val)
    lstr$append(option_pos$new(valnames[idx_val], value = val))

    idx_val <- idx_val + 1L
  }

  if (!rp$is("paren_close")) {
    abort("Bug: should end on closing paren.", "nmrec_dev_error")
  }

  lstr$append(rp$yank())
  rp$gobble(lstr = lstr)
  process_theta_value_option(rp, lstr)
  rp$gobble(lstr = lstr)

  param_parse_x(rp, lstr)
}

check_for_short_unint <- function(x) {
  x <- tolower(x)
  if (identical(x, "u") || identical(x, "un")) {
    abort(
      "nmrec requires 'unint' to be at least three characters.",
      "nmrec_unsupported"
    )
  }
}

#' Find starting index for theta value
#'
#' theta may consist of one to three values, which NONMEM labels as "low",
#' "init", "up". In most cases, the starting index is 1 ("low") but for the
#' one-value form it is 2 ("init").
#'
#' @param rp `record_parser` object.
#' @param pos_end Position of closing paren element.
#' @return Index (1L or 2L) for c("low", "init", "up").
#' @noRd
get_theta_value_idx <- function(rp, pos_end) {
  n_vals <- sum(purrr::map_lgl(rp$elems[rp$idx_e:pos_end], ~ {
    !inherits(.x, "nmrec_element") && is.null(param_get_value_option(.x))
  }))

  if (!n_vals) {
    abort(
      c(
        "Did not find theta value.",
        rp$format()
      ),
      "nmrec_parse_error"
    )
  }

  if (n_vals > 3) {
    abort(
      c(
        "More than three theta values specified.",
        rp$format()
      ),
      "nmrec_parse_error"
    )
  }

  if (identical(n_vals, 1L)) {
    idx_val <- 2L
  } else {
    idx_val <- 1L
  }

  return(idx_val)
}

#' Process options that are tied to a theta initial estimate
#'
#' @param rp `record_parser` object.
#' @param lstr `lstring` object for theta value.
#' @noRd
process_theta_value_option <- function(rp, lstr) {
  rp$walk(function(r) {
    opt <- param_get_value_option(r$current())
    if (!is.null(opt)) {
      lstr$append(option_flag$new(opt, r$yank(), TRUE))
      r$gobble(lstr = lstr)
    }
  })
}

record_theta <- R6::R6Class(
  "nmrec_record_theta",
  inherit = record
)
record_theta$set("private", "parse_fn", parse_theta_record)

theta_option_types <- list(
  "abort" = option_type_flag,
  "names" = option_type_value,
  "noabort" = option_type_flag,
  "noabortfirst" = option_type_flag,
  "numberpoints" = option_type_value
)

theta_option_names <- list2env(
  list(
    "a" = "abort", # NM-TRAN takes this, not sure if nmrec should follow.
    "ab" = "abort",
    "abo" = "abort",
    "abor" = "abort",
    "abort" = "abort",
    "names" = "names",
    "no" = "noabort",
    "noa" = "noabort",
    "noab" = "noabort",
    "noabo" = "noabort",
    "noabor" = "noabort",
    "noabort" = "noabort",
    "noabortfirst" = "noabortfirst",
    "nu" = "numberpoints",
    "num" = "numberpoints",
    "numb" = "numberpoints",
    "numbe" = "numberpoints",
    "number" = "numberpoints",
    "numberp" = "numberpoints",
    "numberpo" = "numberpoints",
    "numberpoi" = "numberpoints",
    "numberpoin" = "numberpoints",
    "numberpoint" = "numberpoints",
    "numberpoints" = "numberpoints",
    "numberpt" = "numberpoints",
    "numberpts" = "numberpoints",
    "nump" = "numberpoints",
    "numpo" = "numberpoints",
    "numpoi" = "numberpoints",
    "numpoin" = "numberpoints",
    "numpoint" = "numberpoints",
    "numpoints" = "numberpoints"
  ),
  parent = emptyenv()
)
