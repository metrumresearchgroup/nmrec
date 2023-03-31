# TODO: Reconsider whether the template/values split is worth keeping.

tstring <- R6::R6Class(
  "nmrec_tstring",
  public = list(
    template = NULL,
    idx_t = 1L,
    values = NULL,
    idx_v = 1L,
    initialize = function(size_template = 30L, size_values = 10L) {
      self$template <- vector("list", size_template)
      self$values <- vector("list", size_values)
    },
    format = function() {
      templ <- self$get_template()
      parts <- purrr::map(templ, ~ {
        if (!identical(length(.x), 1L)) {
          abort(
            c(
              "Got non-scalar for template element.",
              deparse_string(.x)
            ),
            "nmrec_dev_error"
          )
        }

        if (is.character(.x)) {
          value <- .x
        } else if (is.integer(.x)) {
          value <- self$values[[.x]]
          if (is.null(value)) {
            abort(
              sprintf("Template value %s not found", .x),
              "nmrec_dev_error"
            )
          }

          if (!is.character(value)) {
            value <- format(value)
          }
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
    },
    get_template = function() {
      purrr::compact(self$template)
    },
    get_values = function() {
      purrr::compact(self$values)
    },
    append_v = function(name, x) {
      self$maybe_grow("values", self$idx_v)
      self$values[[self$idx_v]] <- x
      names(self$values)[[self$idx_v]] <- name

      self$maybe_grow("template", self$idx_t)
      self$template[[self$idx_t]] <- self$idx_v

      self$tick_v()
      self$tick_t()

      return(invisible(self))
    },
    append_t = function(x) {
      self$maybe_grow("template", self$idx_t)
      self$template[[self$idx_t]] <- x
      self$tick_t()
      return(invisible(self))
    },
    maybe_grow = function(which, idx) {
      l <- self[[which]]
      n <- length(l)
      if (idx > n) {
        self[[which]] <- c(l, vector("list", n))
      }
    },
    tick_v = function(n = 1L) {
      self$idx_v <- self$idx_v + n
      return(invisible(self))
    },
    tick_t = function(n = 1L) {
      self$idx_t <- self$idx_t + n
      return(invisible(self))
    },
    pop_until = function(pred) {
      idx <- purrr::detect_index(
        self$template[seq_len(self$idx_t)],
        pred,
        .dir = "backward"
      )

      if (identical(idx, 0L) || identical(idx, self$idx_t - 1L)) {
        return(list())
      }

      popped <- self$template[(idx + 1):(self$idx_t - 1L)]
      self$template <- self$template[seq_len(idx)]
      self$idx_t <- idx + 1L

      return(popped)
    }
  )
)
