#' Return error class to pass to rlang::abort()
#'
#' @param kind Short label (e.g., "dev") that maps to known nmrec error class
#'   (e.g., "nmrec_dev_error").
#' @noRd
nmrec_error <- function(kind = NULL) {
  base <- "nmrec_error"
  if (is.null(kind)) {
    return(base)
  }

  cls <- switch(kind,
    dev = "nmrec_dev_error",
    parse = "nmrec_parse_error",
    unknown_option = c("nmrec_unknown_option", "nmrec_parse_error"),
    unknown_record = c("nmrec_unknown_record", "nmrec_parse_error"),
    unsupported = "nmrec_unsupported",
    abort(paste("No known error class for `kind`:", deparse_string(kind)))
  )

  return(c(cls, base))
}
