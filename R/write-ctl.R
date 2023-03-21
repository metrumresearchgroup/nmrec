#' Write records to a file.
#'
#' @param records A `nmrec_ctl_records` object.
#' @param path File to write records to.
#' @return `records`, unmodified and invisibly.
#' @seealso [read_ctl()] for reading records from a file.
#'
#' @export
write_ctl <- function(records, path) {
  stopifnot(inherits(records, "nmrec_ctl_records"))
  stopifnot(length(path) == 1, is.character(path), nzchar(path))

  cat(format.nmrec_ctl_records(records), file = path)

  return(invisible(records))
}

#' @export
format.nmrec_ctl_records <- function(x, ...) {
  out <- paste0(purrr::map_chr(x$records, format), collapse = "")
  if (length(x$frontmatter)) {
    out <- paste0(paste0(x$frontmatter, collapse = "\n"), "\n", out)
  }

  return(out)
}
