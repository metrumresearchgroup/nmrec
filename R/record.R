#' Create record object from a record's lines
#'
#' @param lines Character vector of lines for a given record.
#' @return An R6 record object.
#' @noRd
make_record <- function(lines) {
  if (!length(lines)) {
    abort(
      "make_record() called with empty `lines`.",
      "nmrec_dev_error"
    )
  }

  line1 <- lines[1]
  match <- regexec("^[ \t]*\\$([A-Za-z]+)", line1)[[1]]
  if (identical(match[1], -1L)) {
    abort(
      c(
        "First non-whitespace in first line must start with '$'.",
        paste("got:", deparse(line1))
      ),
      "nmrec_dev_error"
    )
  }

  beg <- match[2]
  end <- beg + attr(match, "match.length")[2] - 1
  name_raw <- substr(line1, beg, end)
  tryCatch(name <- canonical_record_name(name_raw),
    nmrec_unknown_record = function(e) {
      warn(
        paste("Unknown record type:", name_raw),
        "nmrec_warning"
      )
      name <<- name_raw
    }
  )

  rec <- switch(name,
    record_raw
  )

  return(rec$new(name = name, name_raw = name_raw, lines = lines))
}

# TODO: Document.
record <- R6::R6Class(
  "nmrec_record",
  public = list(
    name = NULL,
    options = NULL,
    template = NULL,
    initialize = function(name, name_raw, lines) {
      self$name <- name
      private$name_raw <- name_raw
      private$lines <- lines
    },
    get_lines = function() {
      private$lines
    },
    format = function() {
      paste0(paste(private$lines, collapse = "\n"), "\n")
    }
  ),
  private = list(
    name_raw = NULL,
    lines = NULL
  )
)

record_raw <- R6::R6Class(
  "nmrec_record_raw",
  inherit = record
)
