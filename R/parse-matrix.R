parse_matrix_record <- function(name, rp) {
  is_block <- purrr::some(rp$elems, ~ {
    identical(matrix_get_prefix_option(.x), "block")
  })

  if (is_block) {
    rp$process_options(fail_on_unknown = FALSE)
  }
  matrix_process_prefix_option(rp)
  rp$gobble()

  fn <- if (is_block) parse_matrix_block else parse_matrix_diag
  record_parser_map(rp, purrr::partial(fn, name = name))
  rp$elems_assert_done()

  return(rp$get_values())
}

parse_matrix_block <- function(name, rp) {
  record_parser_map(rp, function(r) {
    r$process_options(fail_on_unknown = FALSE)
    param_parse_label(r)
    r$process_options(fail_on_unknown = FALSE)
    if (!r$elems_done()) {
      parse_matrix_block_init(name, r)
      r$process_options(fail_on_unknown = FALSE)
    }
  })
}

parse_matrix_block_init <- function(name, rp) {
  param_append(name, rp, parse_matrix_init(rp))
}

parse_matrix_diag <- function(name, rp) {
  record_parser_map(rp, function(r) {
    param_parse_label(r)
    r$gobble_one("whitespace")
    parse_matrix_diag_init(name, r)
    r$gobble()
  })
}

parse_matrix_diag_init <- function(name, rp) {
  param_append(
    name, rp,
    parse_matrix_init(rp, matrix_process_diag_option)
  )
}

#' Parse matrix init value
#'
#' @param rp `record_parser` object.
#' @param opt_fn A function that consumes value options (e.g., "fixed"). This
#'   will be called with `rp` and the current value `tstring` object. If parsing
#'   a value outside of parentheses, this will be called once after parsing the
#'   main value. If inside of parentheses, before parsing each main value.
#' @return A `tstring` object.
#' @noRd
parse_matrix_init <- function(rp, opt_fn = NULL) {
  lstr <- lstring$new()
  if (rp$elems_is("paren_open")) {
    lstr$append(rp$elems_yank())
    pos_end <- find_closing_paren(rp, "linebreak")
    rp$gobble(lstr = lstr)
    while (rp$idx_e < pos_end) {
      if (!is.null(opt_fn)) {
        opt_fn(rp, lstr)
      }

      if (rp$idx_e < pos_end) {
        lstr$append(option_pos$new("init", value = rp$elems_yank()))
        rp$gobble_one(c("comma", "whitespace"), lstr = lstr)
      }
    }

    if (!rp$elems_is("paren_close")) {
      abort("Bug: should end on closing paren.", "nmrec_dev_error")
    }

    lstr$append(rp$elems_yank())
    rp$gobble_one(c("comma", "whitespace"), lstr = lstr)
    param_parse_x(rp, lstr)
  } else {
    lstr$append(option_pos$new("init", value = rp$elems_yank()))
    rp$gobble(lstr = lstr)
    if (!is.null(opt_fn)) {
      opt_fn(rp, lstr)
    }
  }

  return(lstr)
}

#' Process matrix prefix such as 'BLOCK(2)'
#'
#' @param rp `record_parser` object.
#' @param lstr `lstring` object for parameter value.
#' @noRd
matrix_process_prefix_option <- function(rp) {
  record_parser_map(rp, function(r) {
    # Note: NM-TRAN allows value "(N)" to come on next line, but that's not
    # accounted for here.
    name_raw <- r$elems_current()
    name <- matrix_get_prefix_option(name_raw)

    if (is.null(name)) {
      return(NULL)
    }

    r$tick_e()

    on_ws <- r$elems_is("whitespace")
    has_value <- r$elems_is("paren_open") ||
      (on_ws && r$elems_is("paren_open", pos = r$idx_e + 1))
    if (has_value) {
      end <- find_closing_paren(rp)
      sep <- if (on_ws) as.character(r$elems_yank()) else ""
      r$append(
        option_value$new(
          name, name_raw,
          value = r$elems_yank_to(end), sep = sep
        )
      )
    } else {
      r$append(option_flag$new(name, name_raw = name_raw))
    }
    r$gobble()
  })
}

matrix_get_prefix_option <- function(x) {
  matrix_prefix_options[[tolower(x)]]
}

matrix_prefix_options <- list(
  "blo" = "block",
  "bloc" = "block",
  "block" = "block",
  "dia" = "diagonal",
  "diag" = "diagonal",
  "diago" = "diagonal",
  "diagon" = "diagonal",
  "diagona" = "diagonal",
  "diagonal" = "diagonal",
  "same" = "same"
)

#' Process options that are tied to a specific initial estimate
#'
#' @param rp `record_parser` object.
#' @param lstr `lstring` object for parameter value.
#' @noRd
matrix_process_diag_option <- function(rp, lstr) {
  record_parser_map(rp, function(r) {
    opt <- get0(tolower(r$elems_current()), envir = diag_option_names)
    if (!is.null(opt)) {
      lstr$append(option_flag$new(opt, r$elems_yank(), TRUE))
      r$gobble_one(c("comma", "whitespace"), lstr = lstr)
    }
  })
}
