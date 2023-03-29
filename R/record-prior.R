parse_prior_record <- function() {
  rp <- record_parser$new(
    private$name_raw, private$lines,
    option_types = prior_option_types,
    option_names = prior_option_names
  )

  prev <- private$previous_rec
  if (!is.null(prev)) {
    prev$parse()
  }

  i <- NULL
  while (!identical(i, rp$idx_e)) {
    i <- rp$idx_e
    rp$process_options(fail_on_unknown = FALSE)

    if (rp$elems_is("paren_open")) {
      pos <- rp$elems_find_next(~ elem_is(.x, "paren_close"))
      if (identical(pos, 0L)) {
        abort(
          c("Missing closing paren.", paste(rp$elems, collapse = "")),
          "nmrec_parse_error"
        )
      }

      rp$options_append(
        option_pos$new("clause", value = rp$elems_yank_to(pos))
      )
      rp$gobble()
    }
  }

  if (!rp$elems_done()) {
    abort(
      c(
        paste(
          "Failed to parse prior record:\n",
          paste(rp$elems, collapse = "")
        ),
        "nmrec_parse_error"
      )
    )
  }

  return(list(template = rp$get_template(), options = rp$get_options()))
}

record_prior <- R6::R6Class(
  "nmrec_record_prior",
  inherit = record
)
record_prior$set("private", "parse_fn", parse_prior_record)

prior_option_types <- list(
  "cnt" = option_type_value,
  # TODO: Value is optional.
  "display" = option_type_value,
  "icmax" = option_type_value,
  "ifnd" = option_type_value,
  "iss" = option_type_value,
  "ityp" = option_type_value,
  "ivar" = option_type_value,
  "mode" = option_type_value,
  "nepp" = option_type_value,
  "neps" = option_type_value,
  "neta" = option_type_value,
  "netp" = option_type_value,
  "npexp" = option_type_value,
  "nsam" = option_type_value,
  "ntheta" = option_type_value,
  "nthp" = option_type_value,
  "nwpri" = option_type_flag,
  "plev" = option_type_value,
  "tnpri" = option_type_flag
)

prior_option_names <- list2env(
  list(
    "cnt" = "cnt",
    "dis" = "display",
    "disp" = "display",
    "displ" = "display",
    "displa" = "display",
    "display" = "display",
    "icmax" = "icmax",
    "ifnd" = "ifnd",
    "iss" = "iss",
    "ityp" = "ityp",
    "ivar" = "ivar",
    "mode" = "mode",
    "nepp" = "nepp",
    "neps" = "neps",
    "neta" = "neta",
    "netp" = "netp",
    "npexp" = "npexp",
    "nsam" = "nsam",
    "ntheta" = "ntheta",
    "nthp" = "nthp",
    "nwpri" = "nwpri",
    "plev" = "plev",
    "tnpri" = "tnpri"
  ),
  parent = emptyenv()
)