#' NONMEM control records object
#'
#' @description
#'
#' S3 object representing a NONMEM control stream.  This has two fields:
#'
#'  * `frontmatter`: a character vector of lines, if any, before the first
#'     record
#'
#'  * `records`: a list of [record] objects
#
#' @name nmrec_ctl_records
#' @aliases ctl_records
NULL

#' Read records from a NONMEM control stream
#'
#' `read_ctl()` parses records from a file, and `parse_ctl()` parses them from a
#' character vector.
#'
#' @param path Path to NONMEM control stream.
#' @return An [nmrec_ctl_records] object.
#' @seealso [write_ctl()] for writing records to a file.
#' @examples
#' lines <- nmrec_examples[["bayes1"]]
#' head(lines)
#' ctl <- parse_ctl(lines)
#' names(ctl)
#' head(ctl$records, n = 2)
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
      nmrec_error("unsupported")
    )
  }

  if (any(grepl("\n", lines, fixed = TRUE))) {
    abort("parse_ctl() input must be split on newlines.", nmrec_error())
  }

  # ATTN: Hold off on vectorizing or otherwise optimizing until implementation
  # settles along with tests.
  beg_pos <- grep("^[ \t]*\\$[A-Za-z]", lines)
  n_records <- length(beg_pos)
  if (!n_records) {
    abort("No records found.", nmrec_error("parse"))
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

  last_rec_of_type <- new.env(parent = emptyenv())
  records <- vector("list", n_records)
  for (i in seq_len(n_records)) {
    beg <- beg_pos[i]
    nm <- extract_record_name(lines[beg])
    name <- nm[1]
    name_raw <- nm[2]

    rec <- make_record(
      name, name_raw,
      lines[beg:end_pos[i]],
      previous_rec = get0(name, envir = last_rec_of_type)
    )
    records[[i]] <- rec
    assign(name, rec, envir = last_rec_of_type)
  }

  n_prob_recs <- sum(purrr::map_chr(records, "name") == "problem")
  if (n_prob_recs == 0) {
    abort("No $PROBLEM records found.", nmrec_error("parse"))
  } else if (n_prob_recs > 1) {
    abort(
      "nmrec only supports one $PROBLEM record.",
      nmrec_error("unsupported")
    )
  }

  res <- list(frontmatter = frontmatter, records = records)
  class(res) <- c("nmrec_ctl_records", class(res))
  return(res)
}

extract_record_name <- function(line) {
  match <- regexec("^[ \t]*\\$([A-Za-z]+)", line)[[1]]
  if (identical(match[1], -1L)) {
    bug(
      c(
        "First non-whitespace in first line must start with '$'.",
        paste("got:", deparse_string(line))
      )
    )
  }

  beg <- match[2]
  end <- beg + attr(match, "match.length")[2] - 1
  name_raw <- substr(line, beg, end)
  tryCatch(name <- resolve_record_name(name_raw),
    nmrec_unknown_record = function(e) {
      warn(
        paste("Unknown record type:", name_raw),
        "nmrec_warning"
      )
      name <<- name_raw
    }
  )

  return(c(name, name_raw))
}
