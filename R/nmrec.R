#' @importFrom rlang %||% abort warn
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("self", "private", "super"))
}

# Kludge: Until R 4.3, 'R CMD check' incorrectly flags a package as "not
# imported from" if it's used only outside of functions.
ignore_unused_imports <- function() {
  R6::R6Class
}
