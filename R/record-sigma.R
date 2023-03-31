parse_sigma_record <- function() {
  rp <- record_parser$new(
    private$name_raw, private$lines,
    option_types = matrix_option_types,
    option_names = matrix_option_names
  )

  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  return(parse_matrix_record("sigma", rp))
}

record_sigma <- R6::R6Class(
  "nmrec_record_sigma",
  inherit = record
)
record_sigma$set("private", "parse_fn", parse_sigma_record)
