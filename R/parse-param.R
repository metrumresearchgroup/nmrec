# Common functions for parsing OMEGA, SIGMA, and THETA records

#' Parse label at current position
#'
#' @param rp `record_parser` object.
#' @noRd
param_parse_label <- function(rp) {
  if (rp$elems_is("equal_sign", pos = rp$idx_e + 1)) {
    rp$append(
      option_pos$new("label", value = rp$elems_yank_to(rp$idx_e + 1))
    )
  }
}

#' Parse repeater ("XN") at end value, if any
#'
#' @param rp `record_parser` object.
#' @param lstr `lstring` object for parameter value.
#' @noRd
param_parse_x <- function(rp, lstr) {
  if (!rp$elems_done()) {
    xcand <- tolower(rp$elems_current())
    if (identical(xcand, "x")) {
      xname <- rp$elems_yank()
      if (rp$elems_is("whitespace")) {
        sep <- as.character(rp$elems_yank())
      } else {
        abort(
          c(
            sprintf(
              "Unexpected element (%s) following X.",
              rp$elems_current()
            ),
            rp$format()
          ),
          "nmrec_parse_error"
        )
      }
      xval <- rp$elems_yank()
      lstr$append(
        option_value$new(
          "x",
          name_raw = xname, value = xval, sep = sep
        )
      )
    } else if (isTRUE(grepl("^x[0-9]+$", xcand))) {
      xname <- substr(xcand, 1, 1)
      xval <- substr(xcand, 2, nchar(xcand))
      lstr$append(
        option_value$new(
          "x",
          name_raw = xname, value = xval, sep = ""
        )
      )
      rp$tick_e()
    }
  }

  return(invisible(rp))
}

param_option_names <- list2env(
  list(
    "fix" = "fixed",
    "fixe" = "fixed",
    "fixed" = "fixed",
    # Note: Outside of parentheses, NM-TRAN will accept down to "u", but only to
    # "uni" when inside of parentheses. And for OMEGA/SIGMA, only down to "uni"
    # is accepted whether outside or inside parentheses. For nmrec, require at
    # least three letters.
    "uni" = "unint",
    "unin" = "unint",
    "unint" = "unint"
  ),
  parent = emptyenv()
)

param_get_value_option <- function(x) {
  get0(tolower(x), envir = param_option_names)
}

#' Store parameter in record parser
#'
#' A parameter value is built up as an `lstring` object. Once the object is
#' complete, this function adds it to a `record_parser` object.
#'
#' @param name Name of parameter (e.g., "theta").
#' @param rp `record_parser` object.
#' @param lstr `lstring` object for parameter value.
#'
#' @noRd
param_append <- function(name, rp, lstr) {
  popped <- lstr$pop_until(~ {
    inherits(.x, "nmrec_option") || elem_is(.x, "paren_close")
  })

  param <- option_param$new(
    name,
    values = lstr$get_values()
  )

  rp$append(param)
  for (elem in popped) {
    rp$append(elem)
  }

  return(invisible(rp))
}
