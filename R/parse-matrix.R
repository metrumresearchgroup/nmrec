parse_matrix_record <- function(name, rp) {
  is_block <- purrr::some(rp$elems, function(x) {
    identical(matrix_get_prefix_option(x), "block")
  })

  if (is_block) {
    rp$walk(function(r) {
      # ^ Do inside a walk() call because the order isn't specified and SCALE
      # may be interspersed between other options.
      process_matrix_options(r, fail_on_unknown = FALSE)
      parse_matrix_scale(r)
    })
  }
  matrix_process_prefix_option(rp)
  rp$gobble()

  fn <- if (is_block) parse_matrix_block else parse_matrix_diag
  fn(name, rp)
  rp$assert_done()

  return(rp$get_values())
}

parse_matrix_block <- function(name, rp) {
  rp$walk(function(r) {
    process_matrix_options(r, fail_on_unknown = FALSE)
    param_parse_label(r)
    r$gobble_one("whitespace")
    parse_matrix_scale(r)
  })
  if (!rp$done() && startsWith("values", tolower(rp$current()))) {
    parse_matrix_block_vpair(name, rp)
    process_matrix_options(rp, fail_on_unknown = FALSE)
  } else if (!rp$done()) {
    rp$walk(function(r) {
      parse_matrix_block_init(name, r)
      rp$walk(function(rr) {
        # ^ This inner walk() allows the items to be in any order, including
        # interspersed.
        process_matrix_options(rr, fail_on_unknown = FALSE)
        param_parse_label(rr)
        rr$gobble_one("whitespace")
        parse_matrix_scale(rr)
      })
    })
  }
}

parse_matrix_block_init <- function(name, rp) {
  param_append(name, rp, parse_matrix_init(rp))
}

parse_matrix_diag <- function(name, rp) {
  rp$walk(function(r) {
    rp$walk(function(rr) {
      # ^ This inner walk() allows the items to be in any order, including
      # interspersed.
      param_parse_label(rr)
      rr$gobble_one("whitespace")
      parse_matrix_scale(rr)
    })
    parse_matrix_diag_init(name, r)
    r$gobble()
  })
}

parse_matrix_diag_init <- function(name, rp) {
  param_append(
    name,
    rp,
    parse_matrix_init(rp, matrix_process_diag_option)
  )
}

#' Parse matrix init value
#'
#' @param rp `record_parser` object.
#' @param opt_fn A function that consumes value options (e.g., "fixed"). This
#'   will be called with `rp` and the current value `lstring` object. If parsing
#'   a value outside of parentheses, this will be called once after parsing the
#'   main value. If inside of parentheses, before parsing each main value.
#' @return A `lstring` object.
#' @noRd
parse_matrix_init <- function(rp, opt_fn = NULL) {
  lstr <- lstring$new()
  end <- rp$find_closing_paren("linebreak")
  if (!identical(end, 0L)) {
    lstr$append(rp$yank())
    rp$gobble(lstr = lstr)
    while (rp$idx_e < end) {
      if (!is.null(opt_fn)) {
        opt_fn(rp, lstr)
      }

      if (rp$idx_e < end) {
        param_append_num_opt(lstr, "init", rp$yank())
        rp$gobble_one(c("comma", "whitespace"), lstr = lstr)
      }
    }

    if (!rp$is("paren_close")) {
      bug("Should end on closing paren.")
    }

    lstr$append(rp$yank())
    rp$gobble_one(c("comma", "whitespace"), lstr = lstr)
    param_parse_x(rp, lstr)
  } else {
    param_append_num_opt(lstr, "init", rp$yank())
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
  rp$walk(function(r) {
    # Note: NM-TRAN allows value "(N)" to come on next line, but that's not
    # accounted for here.
    name_raw <- r$current()
    name <- matrix_get_prefix_option(name_raw)

    if (is.null(name)) {
      return(NULL)
    }

    r$tick_e()

    on_ws <- r$is("whitespace")
    has_value <- r$is("paren_open") ||
      (on_ws && r$is("paren_open", pos = r$idx_e + 1))
    if (has_value) {
      sep <- if (on_ws) as.character(r$yank()) else ""
      end <- rp$find_closing_paren()
      r$append(
        option_value$new(
          name,
          name_raw,
          value = r$yank_to(end),
          sep = sep
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
  rp$walk(function(r) {
    opt <- get0(tolower(r$current()), envir = diag_option_names)
    if (!is.null(opt)) {
      lstr$append(option_flag$new(opt, r$yank(), TRUE))
      r$gobble_one(c("comma", "whitespace"), lstr = lstr)
    }
  })

  return(invisible(rp))
}

#' Parse VALUES(diag,odiag) at current position
#' @noRd
parse_matrix_block_vpair <- function(name, rp) {
  lstr <- lstring$new()

  lstr$append(option_flag$new("values", rp$yank(), TRUE))
  rp$gobble_one("whitespace", lstr = lstr)

  if (!rp$is("paren_open")) {
    abort(
      c("Opening paren missing in 'VALUES(diag,odiag)'.", rp$format()),
      nmrec_error("parse")
    )
  }
  lstr$append(rp$yank())
  rp$gobble_one("whitespace", lstr = lstr)

  param_append_num_opt(lstr, "diag", rp$yank())
  rp$gobble_one("whitespace", lstr = lstr)

  if (!rp$is("comma")) {
    abort(
      c("Comma missing in 'VALUES(diag,odiag)'.", rp$format()),
      nmrec_error("parse")
    )
  }
  lstr$append(rp$yank())
  rp$gobble_one("whitespace", lstr = lstr)

  param_append_num_opt(lstr, "odiag", rp$yank())
  rp$gobble_one("whitespace", lstr = lstr)

  if (!rp$is("paren_close")) {
    abort(
      c("Closing paren missing in 'VALUES(diag,odiag)'.", rp$format()),
      nmrec_error("parse")
    )
  }
  lstr$append(rp$yank())

  param_append(name, rp, lstr)
  rp$gobble()
}

parse_matrix_scale <- function(rp) {
  if (rp$done()) {
    return(NULL)
  }

  opt_raw <- rp$current()
  if (!tolower(opt_raw) %in% c("sca", "scal", "scale")) {
    return(NULL)
  }
  if (rp$is("equal_sign", pos = rp$idx_e + 1)) {
    # This is a label, not the SCALE option.
    return(NULL)
  }

  rp$tick_e()

  if (rp$is("paren_open")) {
    sep <- ""
  } else {
    lstr <- lstring$new()
    rp$gobble(lstr)
    if (!rp$is("paren_open")) {
      abort(
        c("SCALE option lacks value:", rp$format()),
        nmrec_error("parse")
      )
    }
    sep <- lstr$format()
  }

  end <- rp$find_closing_paren("linebreak")
  rp$append(
    option_value$new(
      "scale",
      opt_raw,
      value = rp$yank_to(end),
      sep = sep
    )
  )

  rp$gobble()
}
