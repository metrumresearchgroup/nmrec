#' Process a specified set of options
#'
#' Many NONMEM records, at least partially, follow the same syntax for record
#' names, for example
#'
#'     ... MAXEVAL=999 PRINT=2
#'
#' which can be spelled in many different ways, such as
#'
#'     ... max 999,print=2
#'
#' This function is responsible for handling the common processing, leaving
#' record-specific details to each record's parse function.
#'
#' @param rp `record_parser` object.
#' @param option_types A named list mapping the normalized option name to
#'   `option_type_value` or `option_type_flag`.
#' @param option_names An environment that maps all forms of each option name to
#'   its normalized form.
#' @param fail_on_unknown Whether to signal an error if an unknown option is
#'   encountered. In either case, processing is halted with `rp$idx_e`
#'   positioned at the unknown name.
#' @noRd
process_options <- function(rp,
                            option_types, option_names,
                            fail_on_unknown = TRUE) {
  rp$gobble()
  while (!rp$done()) {
    opt_raw <- rp$current()
    opt <- resolve_option(opt_raw, option_names)
    if (is.null(opt)) {
      if (fail_on_unknown) {
        abort(
          c(
            paste("Unknown option:", deparse_string(as.character(opt_raw))),
            format(rp)
          ),
          nmrec_error("unknown_option")
        )
      }
      break
    }
    rp$tick_e()

    kind <- option_types[[opt]]
    if (is.null(kind)) {
      bug(paste("No type defined for", opt))
    }

    if (identical(kind, "flag")) {
      rp$append(option_flag$new(opt, opt_raw, TRUE))
      rp$gobble()
    } else if (identical(kind, "value")) {
      if (rp$is("paren_open")) {
        sep <- ""
      } else {
        beg <- rp$idx_e
        idx_sep <- purrr::detect_index(
          rp$elems[beg:rp$n_elems],
          function(x) !elem_is(x, c("whitespace", "equal_sign"))
        )
        if (idx_sep < 2) {
          abort(
            c(
              paste("Missing value for", opt_raw),
              rp$format()
            ),
            nmrec_error("parse")
          )
        }
        sep <- rp$yank_to(beg + idx_sep - 2)
      }

      end <- rp$find_closing_paren()
      if (!identical(end, 0L)) {
        val <- rp$yank_to(end)
      } else {
        val <- rp$yank(fold_quoted = TRUE)
      }
      rp$append(
        option_value$new(opt, opt_raw, value = val, sep = sep)
      )
      rp$gobble()
    } else {
      bug(paste("Unrecognized type for", opt))
    }
  }

  return(invisible(rp))
}

resolve_option <- function(x, option_names) {
  get0(tolower(x), option_names)
}
