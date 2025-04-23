#' Add or modify a record option
#'
#' Add a new flag or value option or adjust the value/presence of an existing one.
#'
#' @param record An [nmrec_record] object.
#' @param name Name of option to select. Any valid spelling of the option name
#'   is allowed.
#' @param value Value to assign to option. For a value option, the specified
#' value can be anything that can be formatted to a string. For a flag option, any
#' non-`NULL` value results in the name being included in the record. Passing
#' `NULL` drops the option from the displayed record.
#'
#' @seealso [set_param] for setting parameter options from values,
#'   [nmrec_option] for a description of option types
#'
#' @examples
#' ctl <- parse_ctl(nmrec_examples[["bayes1"]])
#' ests <- select_records(ctl, "est")
#' # Get method for first estimation record.
#' set_record_option(ests[[1]], "meth", "nuts")
#' ests[[1]]
#'
#' @export
set_record_option <- function(record, name, value) {
  stopifnot(inherits(record, "nmrec_record"))
  stopifnot(length(name) == 1, is.character(name), nzchar(name))

  name_resolved <- resolve_option_name(record, name)
  opt_prev <- get_record_option(record, name)

  if (is.null(name_resolved)) {
    abort(
      sprintf(
        "%s is not a known flag or value option for %s blocks",
        name,
        record[["name"]]
      ),
      nmrec_error()
    )
  }

  # Create new record option
  if (is.null(opt_prev)) {
    type_map <- switch(
      record[["name"]],
      omega = matrix_option_types,
      sigma = matrix_option_types,
      get(paste0(record[["name"]], "_option_types"))
    )
    opt_type <- type_map[[name_resolved]]

    if (opt_type == "value") {
      opt <- option_value$new(name_resolved, name, value = value)
    } else if (opt_type == "flag") {
      opt <- option_flag$new(name_resolved, name)
    } else {
      bug(paste("No type defined for", name))
    }

    new_opt_lst <- list(elem_whitespace(" "), opt)

    opt_end <- purrr::detect_index(
      record$values,
      function(x) {
        inherits(x, "nmrec_option")
      },
      .dir = "backward"
    )
    record$values <- append(record$values, new_opt_lst, after = opt_end)
  } else {
    # Modify existing option
    opt_prev$value <- value
  }
}
