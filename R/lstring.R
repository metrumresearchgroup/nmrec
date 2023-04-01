#' Build up a string as a list of elements
#'
#' Fields:
#'
#'  * idx: position of the _next_ insertion.
#'
#'  * values: a pre-allocated list for storing elements that will be rendered as
#'    a string. The optional `size` argument specified during initialization
#'    controls how many elements are allocated upfront.
#'
#' Methods:
#'
#'  * format(): render values as a string.
#'
#'  * get_values(): get list of values, dropping any pre-allocated elements that
#'    haven't been populated.
#'
#'  * append(x): add `x` to the end of `values`.
#'
#'  * tick(n): increment `idx` by `n` (1 by default).
#'
#'  * pop_until(pred): drop elements from the end of `values` until `pred`
#'    returns `TRUE`.
#'
#' @noRd
lstring <- R6::R6Class(
  "nmrec_lstring",
  public = list(
    idx = 1L,
    values = NULL,
    initialize = function(size = 30L) {
      self$values <- vector("list", size)
    },
    format = function() {
      lstr_format(self$get_values())
    },
    get_values = function() {
      self$values[seq_len(self$idx - 1)]
    },
    append = function(x) {
      if (self$idx > length(self$values)) {
        self$values <- c(self$values, vector("list", length(self$values)))
      }

      self$values[[self$idx]] <- x
      self$tick()

      return(invisible(self))
    },
    tick = function(n = 1L) {
      self$idx <- self$idx + n
      return(invisible(self))
    },
    pop_until = function(pred) {
      idx <- purrr::detect_index(
        self$values[seq_len(self$idx)],
        pred,
        .dir = "backward"
      )

      if (identical(idx, 0L) || identical(idx, self$idx - 1L)) {
        return(list())
      }

      popped <- self$values[(idx + 1):(self$idx - 1L)]
      self$values <- self$values[seq_len(idx)]
      self$idx <- idx + 1L

      return(popped)
    }
  )
)

lstr_format <- function(values) {
  paste(
    purrr::map_chr(
      values, ~ {
        if (is.character(.x)) {
          .x
        } else {
          format(.x)
        }
      }
    ),
    collapse = ""
  )
}
