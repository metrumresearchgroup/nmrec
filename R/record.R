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
    omega = record_omega,
    prior = record_prior,
    sigma = record_sigma,
    table = record_table,
    theta = record_theta,
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
    values = NULL,
    initialize = function(name, name_raw, lines, previous_rec = NULL) {
      self$name <- name
      private$name_raw <- name_raw
      private$lines <- lines
      private$previous_rec <- previous_rec
    },
    get_options = function() {
      purrr::keep(self$values, ~ inherits(.x, "nmrec_option"))
    },
    get_lines = function() {
      private$lines
    },
    format = function() {
      if (is.null(self$values)) {
        paste0(paste(private$lines, collapse = "\n"), "\n")
      } else {
        lstr_format(self$values)
      }
    },
    parse = function() {
      if (is.null(self$values)) {
        self$values <- private$parse_fn()
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
