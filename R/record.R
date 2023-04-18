#' Create record object from a record's lines
#'
#' @param name Normalized record name.
#' @param name_raw Record name as specified in record.
#' @param lines Character vector of lines for a given record.
#' @return An [nmrec_record] R6 record object.
#' @noRd
make_record <- function(name, name_raw, lines, previous_rec = NULL) {
  if (!length(lines)) {
    bug("make_record() called with empty `lines`.")
  }

  rec <- switch(name,
    data = record_data,
    estimation = record_estimation,
    input = record_input,
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

#' Record objects
#'
#' When [read_ctl()] reads a control stream, it returns an [nmrec_ctl_records]
#' S3 object. Its `records` field is a list of [R6][R6::R6Class] objects that
#' inherits from `nmrec_record`. When parsing _within_ a record is supported for
#' a given record type (e.g., "estimation"), the child class is specific for
#' that type (e.g., `nmrec_record_estimation`). On the other hand, records for
#' which parsing has not been implemented have a type `nmrec_record_raw`.
#'
#' Fields
#' ------
#'
#'   * `name`: the standardized name for the record (e.g., "estimation").
#'
#'   * `values`: a list of capturing the record's content. This is constructed
#'      by the `$parse()` method and consists of "elements" like white space and
#'      line breaks interspersed with [option] objects.
#'
#' Methods
#' -------
#'
#'   * `parse`: parse the record's lines into `values`, enabling inspection and
#'      modification.
#'
#'   * `get_options`: return list of [nmrec_option] objects from `values`.
#'
#'   * `get_lines`: return original `lines` passed during initialization. These
#'      values are never updated.
#'
#'   * `format`: render the record as a string. If the record has been parsed,
#'     this is derived from `values`.
#'
#' @name record
#' @aliases nmrec_record
#' @seealso [select_records] to get records of a given type from an
#'   [nmrec_ctl_records] object.
record <- R6::R6Class(
  "nmrec_record",
  public = list(
    values = NULL,
    initialize = function(name, name_raw, lines, previous_rec = NULL) {
      private$.name <- name
      private$name_raw <- name_raw
      private$lines <- lines
      private$previous_rec <- previous_rec
    },
    get_options = function() {
      self$parse()
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
    .name = NULL,
    name_raw = NULL,
    lines = NULL,
    previous_rec = NULL,
    # `parse_fn` is defined by derived classes. It should return a list of
    # nmrec_option and nmrec_element objects to assign to the `values` field.
    parse_fn = function() {
      abort(
        sprintf(
          "Parsing not implemented for $%s record types.",
          self$name
        ),
        nmrec_error("unsupported")
      )
    }
  ),
  active = list(
    name = function(value) {
      if (missing(value)) {
        private$.name
      } else {
        abort("`name` is read-only", nmrec_error())
      }
    }
  )
)

#' @rdname record
record_raw <- R6::R6Class(
  "nmrec_record_raw",
  inherit = record
)
