#' Create record object from a record's lines
#'
#' @param name Normalized record name.
#' @param name_raw Record name as specified in record.
#' @param lines Character vector of lines for a given record.
#' @return An `nmrec_record` R6 record object.
#' @noRd
make_record <- function(name, name_raw, lines, previous_rec = NULL) {
  if (!length(lines)) {
    abort(
      "make_record() called with empty `lines`.",
      "nmrec_dev_error"
    )
  }

  rec <- switch(name,
    data = record_data,
    estimation = record_estimation,
    table = record_table,
    record_raw
  )

  return(rec$new(
    name = name, name_raw = name_raw,
    lines = lines,
    previous_rec = previous_rec
  ))
}

# TODO: Document.
record <- R6::R6Class(
  "nmrec_record",
  public = list(
    name = NULL,
    options = NULL,
    template = NULL,
    initialize = function(name, name_raw, lines, previous_rec = NULL) {
      self$name <- name
      private$name_raw <- name_raw
      private$lines <- lines
      private$previous_rec <- previous_rec
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
        res <- private$parse_fn()
        self$template <- res[["template"]]
        self$options <- res[["options"]]
      }

      return(invisible(self))
    }
  ),
  private = list(
    name_raw = NULL,
    lines = NULL,
    previous_rec = NULL,
    parse_fn = function() {
      abort(
        sprintf(
          "Parsing not implemented for $%s record types.",
          self$name
        ),
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
