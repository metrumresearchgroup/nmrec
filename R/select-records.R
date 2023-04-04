#' Select records of the specified type
#'
#' An `nmrec_ctl_records` object has all the records of a control stream under
#' its "records" field. This function is a convenience wrapper for selecting
#' records of a given type (e.g., "theta").
#'
#' @param records An `nmrec_ctl_records` object.
#' @param name Get records of this type. This may be spelled any way that's
#'   accepted in the control stream.
#' @return A list of `nmrec_record` objects.
#' @seealso [read_ctl()] for reading records from a file, [get_record_option()]
#'   for extracting an option from a record.
#' @examples
#' ctl <- parse_ctl(get("control3", envir = nmrec_examples))
#' # Get all "$ESTIMATION" records.
#' select_records(ctl, "est")
#'
#' @export
select_records <- function(records, name) {
  stopifnot(inherits(records, "nmrec_ctl_records"))
  stopifnot(length(name) == 1, is.character(name), nzchar(name))

  name_resolved <- resolve_record_name(name)
  purrr::keep(records$records, ~ identical(.x[["name"]], name_resolved))
}
