# TODO: Document.
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

option_param <- R6::R6Class(
  "nmrec_option_param",
  inherit = option_pos,
  public = list(
    template = NULL,
    values = NULL,
    initialize = function(name, template, values) {
      super$initialize(name = name)
      self$template <- template
      self$values <- values
    },
    format = function() {
      if (is.null(self$template)) {
        ""
      } else {
        parts <- purrr::map(self$template, ~ {
          if (inherits(.x, "nmrec_element")) {
            value <- .x
          } else if (identical(length(.x), 1L) && is.integer(.x)) {
            opt <- self$values[[.x]]
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

        paste(parts, collapse = "")
      }
    }
  )
)
