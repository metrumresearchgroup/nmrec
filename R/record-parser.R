# TODO: Implement parsing for a few records and then revisit interface (to
# simplify, extract, rename, ...).

# TODO: Document.
record_parser <- R6::R6Class(
  "nmrec_record_parser",
  public = list(
    name_raw = NULL,
    lines = NULL,
    elems = NULL,
    n_elems = NULL,
    idx_e = 1L,
    options = NULL,
    idx_o = 1L,
    template = NULL,
    idx_t = 1L,
    initialize = function(name_raw, lines) {
      self$name_raw <- name_raw
      self$lines <- lines

      self$elems <- split_to_elements(lines)
      self$n_elems <- length(self$elems)
      self$options <- vector("list", self$n_elems)
      self$template <- vector("list", self$n_elems * 2)

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
      self$template_append("record_name")
      self$gobble_one("whitespace")
    },
    get_template = function() {
      purrr::compact(self$template)
    },
    get_options = function() {
      purrr::compact(self$options)
    },
    options_append = function(x) {
      self$options[[self$idx_o]] <- x
      names(self$options)[[self$idx_o]] <- x$name
      self$tick_o()
      return(invisible(self))
    },
    template_append = function(x) {
      self$template[[self$idx_t]] <- x
      self$tick_t()
      return(invisible(self))
    },
    elems_done = function() {
      return(self$idx_e > self$n_elems)
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
    elems_is = function(types, pos = NULL, which = FALSE) {
      pos <- pos %||% self$idx_e
      if (pos > self$n_elems) {
        return(FALSE)
      }
      return(elem_is(self$elems[[pos]], types, which = which))
    },
    elems_find_next = function(types) {
      beg <- self$idx_e + 1
      n_elems <- self$n_elems
      if (beg > n_elems) {
        return(0L)
      }
      idx <- purrr::detect_index(self$elems[beg:n_elems], ~ elem_is(.x, types))
      if (identical(idx, 0L)) {
        pos <- 0L
      } else {
        pos <- beg + idx - 1
      }

      return(pos)
    },
    gobble_one = function(types) {
      if (self$elems_is(types)) {
        self$template_append(self$elems_yank())
      }
      return(invisible(self))
    },
    gobble_comment = function() {
      if (self$elems_is("semicolon")) {
        beg <- self$idx_e
        lb <- self$elems_find_next("linebreak")
        if (identical(lb, 0L)) {
          abort(
            "Record always end with linebreak element",
            "nmrec_dev_error"
          )
        }
        comment <- elem_comment(
          paste0(self$elems[beg:(lb - 1)], collapse = "")
        )
        self$template_append(comment)
        self$template_append(self$elems[[lb]])
        self$idx_e <- lb + 1
      }
      return(invisible(self))
    },
    gobble = function() {
      uninteresting <- c(
        "ampersand", "comma", "equal_sign",
        "linebreak", "whitespace"
      )
      while (!self$elems_done()) {
        if (self$elems_is("semicolon")) {
          self$gobble_comment()
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

        self$template_append(self$elems_yank())
      }
      return(invisible(self))
    },
    process_options = function(known_options, name_map) {
      process_options(self, known_options, name_map)
      return(invisible(self))
    },
    tick_e = function(n = 1L) {
      self$idx_e <- self$idx_e + n
      return(invisible(self))
    },
    tick_o = function(n = 1L) {
      self$idx_o <- self$idx_o + n
      return(invisible(self))
    },
    tick_t = function(n = 1L) {
      self$idx_t <- self$idx_t + n
      return(invisible(self))
    }
  )
)

process_options <- function(rp, known_options, name_map) {
  rp$gobble()
  while (!rp$elems_done()) {
    opt_raw <- rp$elems_yank()
    opt <- get0(tolower(opt_raw), name_map)
    if (is.null(opt)) {
      abort(
        sprintf("Unknown option for $%s: %s", rp$name_raw, opt_raw),
        c("nmrec_unknown_option", "nmrec_parse_error")
      )
    }

    kind <- known_options[[opt]]
    if (is.null(kind)) {
      abort(paste("No type defined for", opt), "nmrec_dev_error")
    }

    if (identical(kind, "flag")) {
      rp$template_append(opt)
      rp$options_append(option_flag$new(opt, opt_raw, TRUE))
      rp$gobble()
    } else if (identical(kind, "value")) {
      rp$template_append(opt)

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
        pos <- rp$elems_find_next("paren_close")
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
          deparse(paste0(elems[seq_len(end)], collapse = ""))
        ),
        "nmrec_parse_error"
      )
    }
  }

  return(end)
}
