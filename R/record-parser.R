# TODO: Implement parsing for a few records and then revisit interface (to
# simplify, extract, rename, ...).

# TODO: Document.
record_parser <- R6::R6Class(
  "nmrec_record_parser",
  public = list(
    name_raw = NULL,
    elems = NULL,
    n_elems = NULL,
    idx_e = 1L,
    lstr = NULL,
    initialize = function(name_raw, lines) {
      self$name_raw <- name_raw

      self$elems <- split_to_elements(lines)
      self$n_elems <- length(self$elems)
      self$lstr <- lstring$new(self$n_elems)

      self$gobble_one("whitespace")

      rn <- self$yank()
      if (!identical(rn, paste0("$", name_raw))) {
        abort(
          c(
            paste("First element must be", name_raw),
            paste("got:", rn)
          ),
          "nmrec_dev_error"
        )
      }
      self$gobble_one("whitespace")
    },
    format = function() {
      paste(self$elems, collapse = "")
    },
    get_values = function() {
      self$lstr$get_values()
    },
    append = function(x) {
      self$lstr$append(x)
      return(invisible(self))
    },
    done = function() {
      return(self$idx_e > self$n_elems)
    },
    assert_done = function() {
      if (!self$done()) {
        abort(
          c(
            sprintf("Failed to parse %s record.", self$name_raw),
            self$format()
          ),
          "nmrec_parse_error"
        )
      }
    },
    assert_remaining = function() {
      if (self$done()) {
        abort("All elements already consumed.", "nmrec_dev_error")
      }
      return(invisible(self))
    },
    yank_to = function(pos) {
      self$assert_remaining()
      beg <- self$idx_e

      if (!isTRUE(pos >= beg)) {
        abort(
          sprintf(
            "pos (%s) must be at least value of current position (%s)",
            pos, beg
          ),
          "nmrec_dev_error"
        )
      }

      x <- paste0(self$elems[beg:pos], collapse = "")
      self$tick_e(1 + pos - beg)

      return(x)
    },
    yank = function(fold_quoted = FALSE) {
      self$assert_remaining()

      pos <- 0L
      if (identical(fold_quoted, TRUE)) {
        pos <- find_closing_quote(self)
      }

      if (identical(pos, 0L)) {
        x <- self$elems[[self$idx_e]]
        self$tick_e()
      } else {
        x <- self$yank_to(pos)
      }

      return(x)
    },
    tick_e = function(n = 1L) {
      self$idx_e <- self$idx_e + n
      return(invisible(self))
    },
    current = function() {
      return(self$elems[[self$idx_e]])
    },
    is = function(types, pos = NULL) {
      pos <- pos %||% self$idx_e
      if (pos > self$n_elems) {
        return(FALSE)
      }
      return(elem_is(self$elems[[pos]], types))
    },
    find_next = function(pred) {
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
    gobble_one = function(types, lstr = NULL) {
      lstr <- lstr %||% self$lstr
      if (self$is(types)) {
        lstr$append(self$yank())
      }
      return(invisible(self))
    },
    gobble_comment = function(lstr = NULL) {
      lstr <- lstr %||% self$lstr
      if (self$is("semicolon")) {
        beg <- self$idx_e
        lb <- self$find_next(~ elem_is(.x, "linebreak"))
        if (identical(lb, 0L)) {
          abort(
            "Record must end with linebreak element",
            "nmrec_dev_error"
          )
        }
        comment <- elem_comment(
          paste0(self$elems[beg:(lb - 1)], collapse = "")
        )
        lstr$append(comment)
        lstr$append(self$elems[[lb]])
        self$idx_e <- lb + 1
      }
      return(invisible(self))
    },
    gobble = function(lstr = NULL) {
      lstr <- lstr %||% self$lstr
      uninteresting <- c(
        "ampersand", "comma", "comment", "equal_sign",
        "linebreak", "whitespace"
      )
      while (!self$done()) {
        if (self$is("semicolon")) {
          self$gobble_comment(lstr = lstr)
          next
        }
        if (self$is("ampersand")) {
          idx <- self$idx_e
          is_cont <- self$is("linebreak", pos = idx + 1) ||
            (self$is("whitespace", pos = idx + 1) &&
              self$is("linebreak", pos = idx + 2))
          if (!is_cont) {
            break
          }
        }
        if (!self$is(uninteresting)) {
          break
        }

        lstr$append(self$yank())
      }
      return(invisible(self))
    }
  )
)

#' Find closing quote
#'
#' Abort if no closing quote can be found.
#'
#' @param rp `record_parser` object.
#' @return Returns element index for closing quote.
#' @noRd
find_closing_quote <- function(rp) {
  end <- 0L
  if (rp$idx_e < rp$n_elems && rp$is("quote")) {
    quote_type <- if (rp$is("quote_single")) {
      "quote_single"
    } else {
      "quote_double"
    }
    # TODO: Does NONMEM support escaping the quote character?

    end <- rp$find_next(~ elem_is(.x, c(quote_type, "linebreak")))
    if (identical(end, 0L) || !rp$is(quote_type, pos = end)) {
      abort(c("Missing closing quote:", rp$format()), "nmrec_parse_error")
    }
  }

  return(end)
}

#' Find closing paren
#'
#' Abort if no closing paren can be found.
#'
#' @param rp `record_parser` object.
#' @param stop_on_types Element types in addition to "paren_close" to stop on.
#' @return Returns element index for closing paren.
#' @noRd
find_closing_paren <- function(rp, stop_on_types = NULL) {
  types <- c("paren_close", stop_on_types)
  end <- rp$find_next(~ elem_is(.x, types))
  if (identical(end, 0L) || !rp$is("paren_close", pos = end)) {
    abort(c("Missing closing paren.", rp$format()), "nmrec_parse_error")
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
record_parser_walk <- function(rp, fn) {
  fn <- purrr::as_mapper(fn)
  i <- NULL
  while (!identical(i, rp$idx_e)) {
    i <- rp$idx_e

    if (rp$done()) {
      break
    }

    fn(rp)
  }

  return(invisible(rp))
}
