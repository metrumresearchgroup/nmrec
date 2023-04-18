#' Split record into elements and process into an lstring object
#'
#' Fields
#' ------
#'
#'  * elems: list of `nmrec_element` objects and plain strings derived by
#'    passing the `lines` argument to `split_to_elements()`.
#'
#'  * n_elems: the length of `elems`
#'
#'  * idx_e: position of next element to process.
#'
#'  * lstr: an `lstring` object containing the processed values.
#'
#' Methods
#' -------
#'
#'  * format(): render `elems` as a string.
#'
#'  * get_values(): get list of values from `lstr`.
#'
#'  * append(): append an element to `lstr`.
#'
#'  * done(): return `TRUE` if `idx_e` is beyond the last item in `elems`.
#'
#'  * assert_done(): signal a parse error if `idx_e` is not beyond the last item
#'    in `elems`.
#'
#'  * assert_remaining(): signal a dev error if `idx_e` is beyond the last item
#'    in `elems`.
#'
#'  * yank_to(pos): extract `elems` from `idx_e` to `pos` and return the
#'    rendered string, moving `idx_e` to the element after `pos`.
#'
#'  * yank(): extract the `elems` item at `idx_e` and increment `idx_e`. If
#'    `fold_quoted` is `TRUE`, check whether the current element is an
#'    `nmrec_quote` object and extract up to the closing quote.
#'
#'  * tick_e(n): increment `idx_e` by `n` (1 by default).
#'
#'  * current(): return `elems` item at `idx_e`. Unlike `yank()`, this does not
#'    move `idx_e`.
#'
#'  * is(types, pos): return `TRUE` if the `elems` item at `pos` (defaults to
#'    `idx_e`) is an `nmrec_element` type specified in `types`.
#'
#' Methods for finding the position of a downstream element:
#'
#'  * find_next(pred): return the position of the next `elems` item (starting
#'    search at `idx_e`) for which `pred` returns `TRUE`. Return 0L if there is
#'    no match.
#'
#'  * find_closing_quote(): return position of closing quote. If the closing
#'    quote is not found on the current line, signal a parse error. If the
#'    current element is not an `nmrec_quote` object (i.e. there's no opening
#'    quote), return 0L.
#'
#'  * find_closing_paren(stop_on_types): return position of closing parenthesis.
#'    If `stop_on_types` is specified, stop searching if one of these types is
#'    encountered. Signal a parse error if the closing parenthesis is not found.
#'    If the current element is not an `nmrec_paren_open` object, return 0L.
#'
#' Gobble methods are used to yank items from `elems` and store them in `lstr`.
#' All the gobble methods accept an optional `lstr` to use instead of the
#' instances `lstr` field.
#'
#'  * gobble_one(types, lstr): if the current `elems` item is one of the
#'    `nmrec_element` types specified by `types`, yank it from `elems` and store
#'    it as is in `lstr`.
#'
#'  * gobble_comment(lstr): if the current `elems` item is an `nmrec_semicolon`
#'    element, yank the rest of the line from `elems` and store it as an
#'    `nmrec_comment` element in `lstr`.
#'
#'  * gobble(lstr): yank `elems` items and store them in `lstr` until an
#'    "interesting" type is reached. The following objects are always considered
#'    uninteresting: `nmrec_ampersand`, `nmrec_comma`, `nmrec_equal_sign`,
#'    `nmrec_linebreak`, and `nmrec_whitespace`. If a `nmrec_semicolon` object
#'    is encountered, `gobble_comment()` is used to store an `nmrec_comment`
#'    object. Otherwise the elements are stored as is.
#'
#' @noRd
record_parser <- R6::R6Class(
  "nmrec_record_parser",
  public = list(
    elems = NULL,
    n_elems = NULL,
    idx_e = 1L,
    lstr = NULL,
    initialize = function(name, name_raw, lines) {
      self$elems <- split_to_elements(lines)
      self$n_elems <- length(self$elems)
      self$lstr <- lstring$new(self$n_elems)

      self$gobble_one("whitespace")

      rn_opt <- option_record_name$new(name, name_raw)
      rn <- self$yank()
      if (!identical(rn, format(rn_opt))) {
        bug(
          c(
            paste("First element must be", format(rn_opt)),
            paste("got:", rn)
          )
        )
      }
      self$append(rn_opt)
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
        bug(c("Failed to parse record.", self$format()))
      }
    },
    assert_remaining = function() {
      if (self$done()) {
        bug("All elements already consumed.")
      }
      return(invisible(self))
    },
    yank_to = function(pos) {
      self$assert_remaining()
      beg <- self$idx_e

      if (!isTRUE(pos >= beg)) {
        bug(
          sprintf(
            "pos (%s) must be at least value of current position (%s)",
            pos, beg
          )
        )
      }

      x <- paste0(self$elems[beg:pos], collapse = "")
      self$tick_e(1 + pos - beg)

      return(x)
    },
    yank = function(fold_quoted = FALSE) {
      self$assert_remaining()

      end <- 0L
      if (identical(fold_quoted, TRUE)) {
        end <- self$find_closing_quote()
      }

      if (identical(end, 0L)) {
        x <- self$elems[[self$idx_e]]
        self$tick_e()

        if (should_absorb_amp(self)) {
          x <- paste0(x, self$elems[[self$idx_e]])
          self$tick_e()
        }
      } else {
        x <- self$yank_to(end)
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
    find_closing_quote = function() {
      end <- 0L
      if (self$idx_e < self$n_elems && self$is("quote")) {
        quote_type <- if (self$is("quote_single")) {
          "quote_single"
        } else {
          "quote_double"
        }
        # TODO: Does NONMEM support escaping the quote character?

        end <- self$find_next(~ elem_is(.x, c(quote_type, "linebreak")))
        if (identical(end, 0L) || !self$is(quote_type, pos = end)) {
          abort(
            c("Missing closing quote:", self$format()),
            nmrec_error("parse")
          )
        }
      }

      return(end)
    },
    find_closing_paren = function(stop_on_types = NULL) {
      end <- 0L
      if (self$idx_e < self$n_elems && self$is("paren_open")) {
        types <- c("paren_close", stop_on_types)
        end <- self$find_next(~ elem_is(.x, types))
        if (identical(end, 0L) || !self$is("paren_close", pos = end)) {
          abort(
            c("Missing closing paren.", self$format()),
            nmrec_error("parse")
          )
        }
      }

      return(end)
    },
    walk = function(fn) {
      fn <- purrr::as_mapper(fn)
      i <- NULL
      while (!identical(i, self$idx_e)) {
        i <- self$idx_e

        if (self$done()) {
          break
        }

        fn(self)
      }
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
          bug("Record must end with linebreak element.")
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

        if (!self$is(uninteresting)) {
          break
        }

        lstr$append(self$yank())
      }
      return(invisible(self))
    }
  )
)

#' Return `TRUE` if the ampersand at point does _not_ look like a continuation
#' and should be absorbed into the last yanked value.
#' @noRd
should_absorb_amp <- function(rp) {
  if (!rp$is("ampersand")) {
    return(FALSE)
  }

  pos <- rp$find_next(~ elem_is(.x, c("semicolon", "linebreak")))
  if (identical(pos, 0L)) {
    bug("Record must end with linebreak element.")
  }

  return(rp$is("semicolon", pos = pos))
}
