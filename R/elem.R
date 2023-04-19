#' Split up a records lines into sub-parts
#'
#' The goal here is to split at every component that may be relevant for
#' downstream parsing, letting the downstream decide whether some elements
#' should be combined again (e.g., elements that are actually in a trailing
#' comment).
#'
#' @param lines Character vector of lines for a record.
#' @return A list of character scalars, with recognized "elements" marked by
#'   `nmrec_element` subclass.
#' @noRd
split_to_elements <- function(lines) {
  matches <- gregexec("[ \t]+|[,;=&()'\"]", lines)
  ln_delim_idxs <- purrr::map(matches, as.vector)
  ln_offsets <- purrr::map(
    matches,
    function(x) as.vector(attr(x, "match.length")) - 1
  )

  n_lines <- length(lines)
  elems <- vector(
    "list",
    # Double number of delimiter matches to count the delimiter itself and the
    # element (if any) to the left side.
    2 * length(unlist(ln_delim_idxs)) +
      # For each line, also count the non-delimiter element at the end of the
      # string (if any) and an item for the newline.
      2 * n_lines
  )
  elem_idx <- 1

  # ATTN: Hold off on vectorizing or otherwise optimizing until implementation
  # settles along with tests.
  for (ln_idx in seq_along(lines)) {
    ln <- lines[[ln_idx]]
    ln_width <- nchar(ln)
    if (!ln_width) {
      elems[[elem_idx]] <- elem_linebreak()
      elem_idx <- elem_idx + 1
      next
    }

    if (startsWith(ln, ";")) {
      elems[[elem_idx]] <- elem_comment(ln)
      elems[[elem_idx + 1]] <- elem_linebreak()
      elem_idx <- elem_idx + 2
      next
    }

    delim_idxs <- ln_delim_idxs[[ln_idx]]
    offsets <- ln_offsets[[ln_idx]]

    if (identical(delim_idxs[1], -1L)) {
      elems[[elem_idx]] <- ln
      elems[[elem_idx + 1]] <- elem_linebreak()
      elem_idx <- elem_idx + 2
      next
    }

    # Scan along line, extracting one non-delimiter stretch and one delimiter
    # stretch per iteration.
    last_end <- 0
    for (char_idx in seq_along(delim_idxs)) {
      beg <- delim_idxs[char_idx]
      end <- beg + offsets[char_idx]
      if (identical(beg, 1L)) {
        elems[[elem_idx]] <- elem_select(substr(ln, beg, end))
        elem_idx <- elem_idx + 1
        last_end <- end
      } else {
        if (beg - last_end > 1) {
          elems[[elem_idx]] <- substr(ln, last_end + 1, beg - 1)
          elem_idx <- elem_idx + 1
        }
        elems[[elem_idx]] <- elem_select(substr(ln, beg, end))
        elem_idx <- elem_idx + 1
        last_end <- end
      }
    }

    # Extract remaining non-delimiter stretch.
    if (last_end < ln_width) {
      elems[[elem_idx]] <- substr(ln, last_end + 1, ln_width)
      elem_idx <- elem_idx + 1
    }

    elems[[elem_idx]] <- elem_linebreak()
    elem_idx <- elem_idx + 1
  }

  return(elems[seq_len(elem_idx - 1)])
}

elem_select <- function(x) {
  switch(x,
    "=" = elem_equal_sign(),
    ";" = elem_semicolon(),
    "&" = elem_ampersand(),
    "," = elem_comma(),
    "\"" = elem_quote_double(),
    "'" = elem_quote_single(),
    "(" = elem_paren_open(),
    ")" = elem_paren_close(),
    elem_whitespace(x)
  )
}

#' Does object inherit from specified `nmrec_element` classes?
#'
#' @param e Object to check.
#' @param types Short labels for `nmrec_element` class names (e.g., "whitespace"
#'   for `nmrec_whitespace`). These will be transformed to a class name to pass
#'   to the `what` argument of [inherits()].
#' @return Boolean.
#' @noRd
elem_is <- function(e, types) {
  classes <- purrr::map_chr(types, elem_get_class)
  return(inherits(e, classes))
}

elem_get_class <- function(x) {
  switch(x,
    "ampersand" = "nmrec_ampersand",
    "comment" = "nmrec_comment",
    "comma" = "nmrec_comma",
    "equal_sign" = "nmrec_equal_sign",
    "linebreak" = "nmrec_linebreak",
    "paren" = "nmrec_paren",
    "paren_close" = "nmrec_paren_close",
    "paren_open" = "nmrec_paren_open",
    "quote" = "nmrec_quote",
    "quote_double" = "nmrec_quote_double",
    "quote_single" = "nmrec_quote_single",
    "semicolon" = "nmrec_semicolon",
    "whitespace" = "nmrec_whitespace",
    bug(paste("Unknown nmrec_element subclass:", deparse_string(x)))
  )
}

elem_ampersand <- function() {
  structure("&",
    class = c(
      "nmrec_ampersand",
      "nmrec_element",
      "character"
    )
  )
}

elem_comment <- function(x) {
  structure(x,
    class = c(
      "nmrec_comment",
      "nmrec_element",
      "character"
    )
  )
}

elem_comma <- function() {
  structure(",",
    class = c(
      "nmrec_comma",
      "nmrec_element",
      "character"
    )
  )
}

elem_equal_sign <- function() {
  structure("=",
    class = c(
      "nmrec_equal_sign",
      "nmrec_element",
      "character"
    )
  )
}

elem_semicolon <- function() {
  structure(";",
    class = c(
      "nmrec_semicolon",
      "nmrec_element",
      "character"
    )
  )
}

elem_linebreak <- function() {
  structure("\n",
    class = c(
      "nmrec_linebreak",
      "nmrec_element",
      "character"
    )
  )
}

elem_paren_close <- function() {
  structure(")",
    class = c(
      "nmrec_paren_close",
      "nmrec_paren",
      "nmrec_element",
      "character"
    )
  )
}

elem_paren_open <- function() {
  structure("(",
    class = c(
      "nmrec_paren_open",
      "nmrec_paren",
      "nmrec_element",
      "character"
    )
  )
}

elem_quote_double <- function() {
  structure("\"",
    class = c(
      "nmrec_quote_double",
      "nmrec_quote",
      "nmrec_element",
      "character"
    )
  )
}

elem_quote_single <- function() {
  structure("'",
    class = c(
      "nmrec_quote_single",
      "nmrec_quote",
      "nmrec_element",
      "character"
    )
  )
}

elem_whitespace <- function(x) {
  structure(x,
    class = c(
      "nmrec_whitespace",
      "nmrec_element",
      "character"
    )
  )
}
