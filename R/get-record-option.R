#' Extract an option from a record
#'
#' A parsed [nmrec_record] object has a populated "values" field that includes
#' [nmrec_option] objects. This function is a convenience wrapper for grabbing a
#' specified option. If the option occurs multiple times in the record, an error
#' is signaled.
#'
#' Note that a record also stores the record type (e.g., "$EST") as an
#' [nmrec_option] object (specifically an `nmrec_option_record_name` object).
#' This "option" won't usually be of interest, but, if you do want to grab it
#' with `get_record_option()`, you need to pass the full type as `name` (e.g.,
#' "estimation" rather than "est").
#'
#' @param record An [nmrec_record] object.
#' @param name Name of option to select. Any valid spelling of the option name
#'   is allowed.
#' @return An [nmrec_option] object. `NULL` is returned if an option for `name`
#'   is not found in `record`.
#' @seealso [select_records()] selecting records of a given type.
#' @examples
#' ctl <- parse_ctl(nmrec_examples[["bayes1"]])
#' ests <- select_records(ctl, "est")
#' # Get method for first estimation record.
#' meth1 <- get_record_option(ests[[1]], "meth")
#' meth1$value
#'
#' @export
get_record_option <- function(record, name) {
  stopifnot(inherits(record, "nmrec_record"))
  stopifnot(length(name) == 1, is.character(name), nzchar(name))

  name_resolved <- resolve_option_name(record, name)

  if (is.null(name_resolved)) {
    # Take name as the resolved name for an `option_pos` or `option_record_name`
    # option.
    name_resolved <- tolower(name)
  }

  return(get_record_option_impl(record$values, name_resolved, name))
}

#' Resolve option name
#'
#' Looks up the corresponding name map for the specified record and resolves
#' the normalized option name.
#'
#' @param record An [nmrec_record] object.
#' @param name Name of option to select. Any valid spelling of the option name
#'   is allowed.
#'
#' @noRd
resolve_option_name <- function(record, name) {
  # This serves to 1) parse if needed and 2) signal an error if parsing hasn't
  # been implemented for the record type.
  record$parse()

  rtype <- record[["name"]]
  name_map <- switch(rtype,
    omega = matrix_option_names,
    sigma = matrix_option_names,
    get(paste0(rtype, "_option_names"))
  )

  name_lc <- tolower(name)
  name_resolved <- get0(name_lc, name_map)
  if (is.null(name_resolved) && rtype %in% c("omega", "sigma")) {
    # parse-matrix.R handles some options outside of matrix_option_names. Check
    # those too.
    name_resolved <- matrix_prefix_options[[name_lc]]
  }

  return(name_resolved)
}

get_record_option_impl <- function(values, name, name_error = name) {
  opts <- purrr::keep(values, function(x) {
    inherits(x, "nmrec_option") && x[["name"]] == name
  })

  n_opts <- length(opts)
  if (n_opts > 1) {
    abort(
      c(
        sprintf("Record has more than one %s option", name_error),
        purrr::map_chr(opts, format)
      ),
      nmrec_error(),
      call = rlang::caller_env()
    )
  } else if (!n_opts) {
    return(NULL)
  }

  return(opts[[1]])
}
