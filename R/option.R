# TODO: Document.
option <- R6::R6Class(
  "nmrec_option",
  public = list(
    name = NULL,
    name_raw = NULL,
    value = NULL,
    initialize = function(name, name_raw = NULL, value = NULL) {
      self$name <- name
      self$name_raw <- name_raw
      self$value <- value
    }
  )
)

option_pos <- R6::R6Class(
  "nmrec_option_pos",
  inherit = option,
  public = list(
    format = function() {
      if (is.null(self$value)) {
        ""
      } else {
        self$value
      }
    }
  )
)

option_flag <- R6::R6Class(
  "nmrec_option_flag",
  inherit = option,
  public = list(
    format = function() {
      if (is.null(self$value)) {
        ""
      } else {
        self$name_raw
      }
    }
  )
)

option_value <- R6::R6Class(
  "nmrec_option_value",
  inherit = option,
  public = list(
    sep = NULL,
    initialize = function(name, name_raw = NULL, value = NULL, sep = "=") {
      self$name <- name
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
