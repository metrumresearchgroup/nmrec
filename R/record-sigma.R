parse_sigma_record <- function() {
  rp <- record_parser$new("sigma", private$name_raw, private$lines)

  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  return(parse_matrix_record("sigma", rp))
}

#' @rdname record
record_sigma <- R6::R6Class(
  "nmrec_record_sigma",
  inherit = record
)
record_sigma$set("private", "parse_fn", parse_sigma_record)
