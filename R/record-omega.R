parse_omega_record <- function() {
  rp <- record_parser$new("omega", private$name_raw, private$lines)

  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  return(parse_matrix_record("omega", rp))
}

record_omega <- R6::R6Class(
  "nmrec_record_omega",
  inherit = record
)
record_omega$set("private", "parse_fn", parse_omega_record)
