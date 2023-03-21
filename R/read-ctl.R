#' Read records from a NONMEM control stream
#'
#' `read_ctl()` parses records from a file, and `parse_ctl()` parses them from a
#' character vector.
#'
#' @param path Path to NONMEM control stream.
#' @return A `nmrec_ctl_records` object.
#' @seealso [write_ctl()] for writing records to a file.
#'
#' @export
read_ctl <- function(path) {
  stopifnot(length(path) == 1, is.character(path), nzchar(path))
  parse_ctl(readLines(path, warn = FALSE))
}

#' @rdname read_ctl
#' @param lines Lines from a NONMEM control stream.
#' @export
parse_ctl <- function(lines) {
  if (any(grepl("^[ \t]*\\$?include", lines, ignore.case = TRUE))) {
    abort(
      "nmrec does not support control streams that use `include`.",
      "nmrec_unsupported"
    )
  }

  # ATTN: Hold off on vectorizing or otherwise optimizing until implementation
  # settles along with tests.
  beg_pos <- grep("^[ \t]*\\$[A-Za-z]", lines)
  n_records <- length(beg_pos)
  if (!n_records) {
    abort("No records found.", "nmrec_parse_error")
  }

  end_pos <- if (identical(n_records, 1L)) {
    length(lines)
  } else {
    c(beg_pos[2:n_records] - 1, length(lines))
  }

  frontmatter <- if (identical(beg_pos[1], 1L)) {
    character(0)
  } else {
    lines[1:beg_pos[1] - 1]
  }

  records <- vector("list", n_records)
  for (i in seq_len(n_records)) {
    records[[i]] <- make_record(lines[beg_pos[i]:end_pos[i]])
  }

  n_prob_recs <- sum(purrr::map_chr(records, "name") == "problem")
  if (n_prob_recs == 0) {
    abort("No $PROBLEM records found.", "nmrec_parse_error")
  } else if (n_prob_recs > 1) {
    abort("nmrec only supports one $PROBLEM record.", "nmrec_unsupported")
  }

  res <- list(frontmatter = frontmatter, records = records)
  class(res) <- c("nmrec_ctl_records", class(res))
  return(res)
}
