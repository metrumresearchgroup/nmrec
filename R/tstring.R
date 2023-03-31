# TODO: Reconsider whether the template/values split is worth keeping.

tstring <- R6::R6Class(
  "nmrec_tstring",
  public = list(
    template = NULL,
    idx_t = 1L,
    values = NULL,
    idx_v = 1L,
    initialize = function(size_template, size_values) {
      self$template <- vector("list", size_template)
      self$values <- vector("list", size_values)
    },
    get_template = function() {
      purrr::compact(self$template)
    },
    get_values = function() {
      purrr::compact(self$values)
    },
    append_v = function(name, x) {
      self$values[[self$idx_v]] <- x
      names(self$values)[[self$idx_v]] <- name
      self$template[[self$idx_t]] <- self$idx_v

      self$tick_v()
      self$tick_t()

      return(invisible(self))
    },
    append_t = function(x) {
      self$template[[self$idx_t]] <- x
      self$tick_t()
      return(invisible(self))
    },
    tick_v = function(n = 1L) {
      self$idx_v <- self$idx_v + n
      return(invisible(self))
    },
    tick_t = function(n = 1L) {
      self$idx_t <- self$idx_t + n
      return(invisible(self))
    }
  )
)
