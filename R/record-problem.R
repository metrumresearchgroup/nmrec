parse_problem_record <- function() {
  rp <- record_parser$new("problem", private$name_raw, private$lines)
  # Note: if the previous element isn't whitespace, it's a bare record name
  # without trailing whitespace or problem text.
  if (rp$is("whitespace", rp$idx_e - 1)) {
    if (rp$is("linebreak")) {
      text <- ""
    } else {
      # NONMEM takes all text on the first line as problem text, even text
      # following a semicolon.
      lb <- rp$find_next(function(x) elem_is(x, "linebreak"))
      if (identical(lb, 0L)) {
        bug("Record must end with linebreak element.")
      }
      text <- rp$yank_to(lb - 1)
    }
    rp$append(option_pos$new("text", value = text))
  }

  rp$walk(parse_problem_tail)
  if (!rp$done()) {
    abort(
      c("Problem text must be on same line as record name.", rp$format()),
      nmrec_error("parse")
    )
  }

  return(rp$get_values())
}

parse_problem_tail <- function(rp) {
  if (rp$is("semicolon")) {
    rp$gobble_comment()
  } else if (rp$is(c("ampersand", "comment", "linebreak", "whitespace"))) {
    rp$append(rp$yank())
  }
}

#' @rdname record
record_problem <- R6::R6Class(
  "nmrec_record_problem",
  inherit = record
)
record_problem$set("private", "parse_fn", parse_problem_record)

# Define option variables for consistency with other records.
problem_option_types <- list()
problem_option_names <- emptyenv()
