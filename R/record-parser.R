# TODO: Implement parsing for a few records and then revisit interface (to
# simplify, extract, rename, ...).

# TODO: Document.
record_parser <- R6::R6Class(
  "nmrec_record_parser",
  public = list(
    name_raw = NULL,
    lines = NULL,
    option_types = NULL,
    option_names = NULL,
    elems = NULL,
    n_elems = NULL,
    idx_e = 1L,
    template = NULL,
    initialize = function(name_raw, lines,
                          option_types = NULL,
                          option_names = NULL) {
      self$name_raw <- name_raw
      self$lines <- lines
      self$option_types <- option_types
      self$option_names <- option_names

      self$elems <- split_to_elements(lines)
      self$n_elems <- length(self$elems)
      self$template <- tstring$new(self$n_elems * 2, self$n_elems)

      self$gobble_one("whitespace")

      rn <- self$elems_yank()
      if (!identical(rn, paste0("$", name_raw))) {
        abort(
          c(
            paste("First element must be", name_raw),
            paste("got:", rn)
          ),
          "nmrec_dev_error"
        )
      }
      self$template$append_t("record_name")
      self$gobble_one("whitespace")
    },
    get_template = function() {
      self$template$get_template()
    },
    get_options = function() {
      self$template$get_values()
    },
    options_append = function(x) {
      self$template$append_v(x$name, x)
      return(invisible(self))
    },
    resolve_option = function(x) {
      get0(tolower(x), self$option_names)
    },
    elems_done = function() {
      return(self$idx_e > self$n_elems)
    },
    elems_assert_done = function() {
      if (!self$elems_done()) {
        abort(
          c(
            sprintf("Failed to parse %s record.", self$name_raw),
            paste(self$elems, collapse = "")
          ),
          "nmrec_parse_error"
        )
      }
    },
    elems_assert_remaining = function() {
      if (self$elems_done()) {
        abort("All elements already consumed.", "nmrec_dev_error")
      }
      return(invisible(self))
    },
    elems_yank_to = function(pos) {
      self$elems_assert_remaining()
      beg <- self$idx_e

      if (!isTRUE(pos >= beg)) {
        abort(
          sprintf(
            "pos (%s) must be at value of current position (%s)",
            pos, beg
          ),
          "nmrec_dev_error"
        )
      }

      x <- paste0(self$elems[beg:pos], collapse = "")
      self$tick_e(1 + pos - beg)

      return(x)
    },
    elems_yank = function(fold_quoted = FALSE) {
      self$elems_assert_remaining()

      offset <- 1L
      if (identical(fold_quoted, TRUE)) {
        offset <- find_closing_quote(self$elems[self$idx_e:self$n_elems])
      }

      if (identical(offset, 1L)) {
        x <- self$elems[[self$idx_e]]
        self$tick_e()
      } else {
        x <- self$elems_yank_to(self$idx_e + offset - 1L)
      }

      return(x)
    },
    elems_current = function() {
      return(self$elems[[self$idx_e]])
    },
    elems_is = function(types, pos = NULL, which = FALSE) {
      pos <- pos %||% self$idx_e
      if (pos > self$n_elems) {
        return(FALSE)
      }
      return(elem_is(self$elems[[pos]], types, which = which))
    },
    elems_find_next = function(pred) {
      beg <- self$idx_e + 1
      n_elems <- self$n_elems
      if (beg > n_elems) {
        return(0L)
      }
      idx <- purrr::detect_index(self$elems[beg:n_elems], pred)
      if (identical(idx, 0L)) {
        pos <- 0L
      } else {
        pos <- beg + idx - 1
      }

      return(pos)
    },
    gobble_one = function(types, template = NULL) {
      templ <- template %||% self$template
      if (self$elems_is(types)) {
        templ$append_t(self$elems_yank())
      }
      return(invisible(self))
    },
    gobble_comment = function(template = NULL) {
      templ <- template %||% self$template
      if (self$elems_is("semicolon")) {
        beg <- self$idx_e
        lb <- self$elems_find_next(~ elem_is(.x, "linebreak"))
        if (identical(lb, 0L)) {
          abort(
            "Record must end with linebreak element",
            "nmrec_dev_error"
          )
        }
        comment <- elem_comment(
          paste0(self$elems[beg:(lb - 1)], collapse = "")
        )
        templ$append_t(comment)
        templ$append_t(self$elems[[lb]])
        self$idx_e <- lb + 1
      }
      return(invisible(self))
    },
    gobble = function(template = NULL) {
      templ <- template %||% self$template
      uninteresting <- c(
        "ampersand", "comma", "comment", "equal_sign",
        "linebreak", "whitespace"
      )
      while (!self$elems_done()) {
        if (self$elems_is("semicolon")) {
          self$gobble_comment(template = templ)
          next
        }
        if (self$elems_is("ampersand")) {
          idx <- self$idx_e
          is_cont <- self$elems_is("linebreak", pos = idx + 1) ||
            (self$elems_is("whitespace", pos = idx + 1) &&
              self$elems_is("linebreak", pos = idx + 2))
          if (!is_cont) {
            break
          }
        }
        if (!self$elems_is(uninteresting)) {
          break
        }

        templ$append_t(self$elems_yank())
      }
      return(invisible(self))
    },
    process_options = function(fail_on_unknown = TRUE) {
      process_options(self, fail_on_unknown = fail_on_unknown)
      return(invisible(self))
    },
    tick_e = function(n = 1L) {
      self$idx_e <- self$idx_e + n
      return(invisible(self))
    }
  )
)

process_options <- function(rp, fail_on_unknown = TRUE) {
  rp$gobble()
  while (!rp$elems_done()) {
    opt_raw <- rp$elems_current()
    opt <- rp$resolve_option(opt_raw)
    if (is.null(opt)) {
      if (fail_on_unknown) {
        abort(
          sprintf("Unknown option for $%s: %s", rp$name_raw, opt_raw),
          c("nmrec_unknown_option", "nmrec_parse_error")
        )
      }
      break
    }
    rp$tick_e()

    kind <- rp$option_types[[opt]]
    if (is.null(kind)) {
      abort(paste("No type defined for", opt), "nmrec_dev_error")
    }

    if (identical(kind, "flag")) {
      rp$options_append(option_flag$new(opt, opt_raw, TRUE))
      rp$gobble()
    } else if (identical(kind, "value")) {
      if (rp$elems_is("paren_open")) {
        sep <- ""
      } else {
        beg <- rp$idx_e
        idx_sep <- purrr::detect_index(
          rp$elems[beg:rp$n_elems],
          ~ !elem_is(.x, c("whitespace", "equal_sign"))
        )
        if (idx_sep < 2) {
          abort(
            c(
              paste("Missing value for", opt_raw),
              paste(rp$elems, collapse = "")
            ),
            "nmrec_parse_error"
          )
        }
        sep <- rp$elems_yank_to(beg + idx_sep - 2)
      }

      if (rp$elems_is("paren_open")) {
        pos <- rp$elems_find_next(~ elem_is(.x, "paren_close"))
        if (identical(pos, 0L)) {
          abort(
            c("Missing closing paren.", paste(rp$elems, collapse = "")),
            "nmrec_parse_error"
          )
        }
        val <- rp$elems_yank_to(pos)
      } else {
        val <- rp$elems_yank(fold_quoted = TRUE)
      }
      rp$options_append(
        option_value$new(opt, opt_raw, value = val, sep = sep)
      )
      rp$gobble()
    } else {
      abort(paste("Unrecognized type for", opt), "nmrec_dev_error")
    }
  }
}

find_closing_quote <- function(elems) {
  n_elems <- length(elems)
  end <- 1L
  if (n_elems > 1 && elem_is(elems[[1]], "quote")) {
    quote_type <- if (elem_is(elems[[1]], "quote_single")) {
      "quote_single"
    } else {
      "quote_double"
    }
    # TODO: Does NONMEM support escaping the quote character?
    end <- 1 + purrr::detect_index(
      elems[2:n_elems],
      ~ elem_is(.x, c(quote_type, "linebreak"))
    )
    if (identical(end, 1L) || !elem_is(elems[[end]], quote_type)) {
      abort(
        paste(
          "Missing closing quote:",
          deparse_string(paste0(elems[seq_len(end)], collapse = ""))
        ),
        "nmrec_parse_error"
      )
    }
  }

  return(end)
}

#' Process record elements with a function
#'
#' Call a function with `rp`, stopping once the element index no longer changes
#' or the index is beyond the last element.
#'
#' @param rp `record_parser` object.
#' @param fn Function to apply. It is called with `rp` as its only argument.
#'
#' @noRd
record_parser_map <- function(rp, fn) {
  fn <- purrr::as_mapper(fn)
  i <- NULL
  while (!identical(i, rp$idx_e)) {
    i <- rp$idx_e

    if (rp$elems_done()) {
      break
    }

    fn(rp)
  }
}
