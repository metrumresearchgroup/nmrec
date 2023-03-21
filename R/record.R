#' Create record object from a record's lines
#'
#' @param lines Character vector of lines for a given record.
#' @return An R6 record object.
#' @noRd
make_record <- function(lines) {
  if (!length(lines)) {
    abort(
      "make_record() called with empty `lines`.",
      "nmrec_dev_error"
    )
  }

  line1 <- lines[1]
  match <- regexec("^[ \t]*\\$([A-Za-z]+)", line1)[[1]]
  if (identical(match[1], -1L)) {
    abort(
      c(
        "First non-whitespace in first line must start with '$'.",
        paste("got:", deparse_string(line1))
      ),
      "nmrec_dev_error"
    )
  }

  beg <- match[2]
  end <- beg + attr(match, "match.length")[2] - 1
  name_raw <- substr(line1, beg, end)
  tryCatch(name <- resolve_record_name(name_raw),
    nmrec_unknown_record = function(e) {
      warn(
        paste("Unknown record type:", name_raw),
        "nmrec_warning"
      )
      name <<- name_raw
    }
  )

  rec <- switch(name,
    data = record_data,
    record_raw
  )

  return(rec$new(name = name, name_raw = name_raw, lines = lines))
}

# TODO: Document.
record <- R6::R6Class(
  "nmrec_record",
  public = list(
    name = NULL,
    options = NULL,
    template = NULL,
    initialize = function(name, name_raw, lines) {
      self$name <- name
      private$name_raw <- name_raw
      private$lines <- lines
    },
    get_lines = function() {
      private$lines
    },
    format = function() {
      if (is.null(self$template)) {
        paste0(paste(private$lines, collapse = "\n"), "\n")
      } else {
        format_from_template(private$name_raw, self$template, self$options)
      }
    },
    parse = function() {
      if (is.null(self$template)) {
        res <- private$parse_fn(private$name_raw, private$lines)
        self$template <- res[["template"]]
        self$options <- res[["options"]]
      }

      return(invisible(self))
    }
  ),
  private = list(
    name_raw = NULL,
    lines = NULL,
    parse_fn = function(name_raw, lines) {
      abort(
        paste("parse_fn not implemented for", deparse_string(class(self))),
        "nmrec_unsupported"
      )
    }
  )
)

record_raw <- R6::R6Class(
  "nmrec_record_raw",
  inherit = record
)

format_from_template <- function(record_name, template, options) {
  parts <- purrr::map(template, ~ {
    if (identical(.x, "record_name")) {
      value <- paste0("$", record_name)
    } else if (inherits(.x, "nmrec_element")) {
      value <- .x
    } else if (identical(length(.x), 1L) && is.integer(.x)) {
      opt <- options[[.x]]
      if (is.null(opt)) {
        abort(
          sprintf("Template element %s not found", .x),
          "nmrec_dev_error"
        )
      }
      value <- opt$format()
    } else {
      abort(
        c(
          "Got unexpected value for template element.",
          deparse_string(.x)
        ),
        "nmrec_dev_error"
      )
    }

    return(value)
  })

  return(paste(parts, collapse = ""))
}
