parse_input_record <- function() {
  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  rp <- record_parser$new("input", private$name_raw, private$lines)
  rp$walk(function(r) {
    named <- r$is("equal_sign", pos = r$idx_e + 1) ||
      (r$is("whitespace", pos = r$idx_e + 1) &&
        r$is("equal_sign", pos = r$idx_e + 2))

    if (named) {
      name <- r$yank()
      idx <- r$find_next(
        function(x) !elem_is(x, c("whitespace", "equal_sign"))
      )
      if (identical(idx, 0L)) {
        bug(paste("Equal sign must follow:", r$format()))
      }

      sep <- r$yank_to(idx - 1)
      val <- r$yank()
      opt <- option_value$new(name, name, value = val, sep = sep)
    } else {
      val <- r$yank()
      opt <- option_flag$new(val, val, TRUE)
    }
    r$append(opt)
    r$gobble()
  })
  rp$assert_done()

  return(rp$get_values())
}

#' @rdname record
record_input <- R6::R6Class(
  "nmrec_record_input",
  inherit = record
)
record_input$set("private", "parse_fn", parse_input_record)
