#' Option objects
#'
#' Parsing a [record] populates its `values` field with elements and options. An
#' option is an [R6][R6::R6Class] object that inherit from `nmrec_option`.
#'
#' Types of options
#' ----------------
#'
#'  * **positional** (`nmrec_option_pos`): the option is rendered as the `value`
#'    field.
#'
#'    For example, the file name from a `$DATA` record with a value of
#'    "foo.csv" would be rendered as "foo.csv".
#'
#'  * **flag** (`nmrec_option_flag`): if `value` is not `NULL`, the option is
#'    rendered as the `name_raw` field.
#'
#'    For example, the interaction flag for an `$ESTIMATION` record with a
#'    `name_raw` value of "interact" and a value of `TRUE` would be rendered as
#'    "interact".
#'
#'  * **value** (`nmrec_option_value`): render name-value pair from `name_raw`
#'    field and `value` field, separating the values by the `sep` field value.
#'
#'    For example, the maxevals option for a `$ESTIMATION` record with a
#'    `name_raw` field of "MAX", a `value` field of "999", and `sep` field of
#'    " = " would be rendered as "MAX = 999".
#'
#'  * **record name** (`nmrec_option_record_name`): this option derives from
#'    `nmrec_option_flag` and tailors formatting for record names.
#'
#'    For example, a `name_raw` value of "est" for an `$ESTIMATION` record would
#'    be rendered as "$est".
#'
#'  * **nested** (`nmrec_option_nested`): this option derives from
#'    `nmrec_option_pos`, replacing the simpler `value` field with a `values`
#'    field that is a list of items to render. Like the `values` field of a
#'    [record] objects, this consists of elements and options. It is useful for
#'    `THETA`, `OMEGA`, and `SIGMA` values, which can have more complex
#'    formatting and have attached options (such as "FIXED").
#'
#' @name option
#' @aliases nmrec_option
#' @seealso [get_record_option()] for getting a specified option from a [record]
#'   object.
option <- R6::R6Class(
  "nmrec_option",
  public = list(
    name = NULL,
    name_raw = NULL,
    value = NULL,
    initialize = function(name, ...) {
      self$name <- name
    }
  )
)

#' @rdname option
option_pos <- R6::R6Class(
  "nmrec_option_pos",
  inherit = option,
  public = list(
    initialize = function(name, value = NULL, ...) {
      super$initialize(name = name)
      self$value <- value
    },
    format = function() {
      if (is.null(self$value)) {
        ""
      } else {
        self$value
      }
    }
  )
)

#' @rdname option
option_flag <- R6::R6Class(
  "nmrec_option_flag",
  inherit = option,
  public = list(
    initialize = function(name, name_raw = NULL, value = TRUE, ...) {
      super$initialize(name = name)
      self$name_raw <- name_raw
      self$value <- value
    },
    format = function() {
      if (is.null(self$value)) {
        ""
      } else {
        self$name_raw
      }
    }
  )
)

#' @rdname option
option_value <- R6::R6Class(
  "nmrec_option_value",
  inherit = option,
  public = list(
    sep = NULL,
    initialize = function(name, name_raw = NULL, value = NULL,
                          sep = "=", ...) {
      super$initialize(name = name)
      self$name_raw <- name_raw
      self$sep <- sep
      self$value <- value
    },
    format = function() {
      if (is.null(self$value)) {
        ""
      } else {
        paste0(self$name_raw, self$sep, self$value)
      }
    }
  )
)

#' @rdname option
option_record_name <- R6::R6Class(
  "nmrec_option_record_name",
  inherit = option_flag,
  public = list(
    format = function() {
      paste0("$", self$name_raw)
    }
  )
)

#' @rdname option
option_nested <- R6::R6Class(
  "nmrec_option_nested",
  inherit = option_pos,
  public = list(
    values = NULL,
    initialize = function(name, values) {
      super$initialize(name = name)
      self$values <- values
    },
    format = function() {
      if (is.null(self$values)) {
        ""
      } else {
        lstr_format(self$values)
      }
    }
  )
)
