# TODO: Consider making print (and perhaps format) nicer for interactive context
# (e.g., not dumping long file to console).

#' @export
print.nmrec_ctl_records <- function(x, ...) {
  cat(format(x, ...))
  return(invisible(x))
}
