#' Create index with positions and details about a parameter
#'
#' The purpose of this index is to allow the caller to get from a position in
#' the parameter space (e.g., THETA[3]) to the corresponding option in
#' `records`.
#'
#' The return value is a list with the following elements:
#'
#'  * name: name of the parameter {"theta", "omega", "sigma"}
#'
#'  * records: list of `nmrec_record` objects for `name`
#'
#'  * details: list of information for each item in `records`
#'
#'    Each element is a list with the following fields:
#'
#'     * type: NA (theta), diagonal, block
#'
#'     *  subtype:
#'
#'        * NA: theta
#'
#'        * plain: type (and thus size) declared explicitly and initial values
#'          listed
#'
#'        * implicit: no type (and thus size) declared, leading to type DIAGONAL
#'          with size inferred from the number of initial estimates
#'
#'        * no_inits: no initial estimates present
#'
#'        * same: block includes SAME option
#'
#'        * vpair: block includes VALUES(diag,odiag) option
#'
#'     * size: size of the parameter values covered by this record. For matrix
#'       parameters, it corresponds to the size along the diagonal.
#'
#'       Examples:
#'
#'        * "$THETA 0.1 0.1" => 2
#'
#'        * "$THETA (0.01,0.1,0.5)x4" => 4
#'
#'        * "$OMEGA 0.1 0.1 0.1" => 3
#'
#'        * "$OMEGA DIAG(2) 0.1 0.1" => 2
#'
#'        * "$OMEGA BLOCK(3) (0.1)x6" => 3
#'
#'     * popts: list of parameters options (i.e. initial estimates captured by
#'       nested options for `name`) present in the record.
#'
#'       A popt typically contains a single "init" option, with three
#'       exceptions:
#'
#'        1) BLOCK records may have more than one "init" option (e.g.,
#'           "(0.1 0.2)x6" would have two).
#'
#'        2) A BLOCK record with a VALUES(diag,odiag) pair has "diag" and
#'           "odiag" options.
#'
#'        3) a theta initial estimate in the form (low,,hi) will not have a
#'           "init" option.
#'
#'  * ltri_to_opt: environment that maps the lower triangle index (including the
#'    diagonal) to the corresponding option
#'
#'    The key is "p{N}", where N is the matrix index. For example, a 3x3 matrix
#'    would have the following indices:
#'
#'         p1   .   .
#'         p2  p3   .
#'         p4  p5  p6
#'
#'    The value is a list with the following fields:
#'
#'     * opt: the option object for the given index. This will usually be an
#'       `nmrec_option_pos` object for "init", "diag", or "odiag". For the
#'       (low,,hi) form of theta, it's the parameter option (i.e. the
#'       `nmrec_option_nested` object).
#'
#'     * popt_index: location of associated parameter option in `popts`
#'
#'     * record_index: location of associated record in `records`
#'
#'  * size: the full size of the parameter. For matrix parameters, it's the size
#'    of the diagonal.
#'
#' @param records An `nmrec_ctl_records` object.
#' @param name Parameter to index ("theta", "omega", "sigma").
#'
#' @noRd
create_param_index <- function(records, name) {
  recs <- purrr::keep(
    records[["records"]],
    function(x) identical(x[["name"]], name)
  )
  fn <- switch(name,
    "theta" = theta_index,
    "omega" = matrix_index,
    "sigma" = matrix_index,
    bug(paste("Unrecognized name:", deparse_string(name)))
  )

  for (r in recs) {
    r$parse()
  }

  return(fn(recs, name))
}

theta_index <- function(records, name) {
  details <- vector("list", length(records))
  map <- new.env(parent = emptyenv())
  vec_idx <- 1L

  for (ridx in seq_along(records)) {
    rec <- records[[ridx]]
    popts <- param_options(rec)
    if (!length(popts)) {
      abort(c("Record has no initial estimates.", rec$format()), nmrec_error())
    }
    # Note: This does not look _into_ each nested theta option. Unlike OMEGA, it
    # doesn't seem like multiple initial estimates for theta can be grouped in
    # "(...)xN". NM-TRAN aborts on "((low,init) (low,init))x2" and treats
    # "(value1 value2)x2" as "(low init)x2".
    sizes <- purrr::map_int(popts, param_x)

    for (oidx in seq_along(popts)) {
      key <- param_format_key(vec_idx)
      popt <- popts[[oidx]]
      init <- get_record_option_impl(popt$values, "init")
      if (is.null(init)) {
        # Special case: for the (low,,hi) form, there's no init option to store.
        opt <- popt
      } else {
        opt <- init
      }
      map[[key]] <- list(opt = opt, popt_index = oidx, record_index = ridx)
      vec_idx <- vec_idx + sizes[oidx]
    }

    details[[ridx]] <- list(
      type = NA_character_,
      subtype = NA_character_,
      size = sum(sizes),
      popts = popts
    )
  }

  return(list(
    name = name,
    records = records,
    details = details,
    ltri_to_opt = map,
    size = sum(purrr::map_int(details, "size"))
  ))
}

matrix_index <- function(records, name) {
  prev <- NULL
  details <- vector("list", length(records))
  for (ridx in seq_along(records)) {
    rec <- records[[ridx]]
    res <- matrix_classify(rec, prev)
    matrix_verify_size(res, rec)
    details[[ridx]] <- res
    prev <- res
  }

  size <- sum(purrr::map_int(details, "size"))
  return(list(
    name = name,
    records = records,
    details = details,
    ltri_to_opt = matrix_create_map(details, size),
    size = size
  ))
}

#' Generate `details` element for a record
#'
#' @param record `nmrec_record` object.
#' @param prev `details` element for previous record of same type, if any.
#' @noRd
matrix_classify <- function(record, prev) {
  if (is.null(get_record_option_impl(record$values, "block"))) {
    fn <- matrix_classify_diagonal
  } else {
    fn <- matrix_classify_block
  }

  fn(record, prev)
}

matrix_classify_diagonal <- function(record, prev) {
  popts <- param_options(record)
  diag <- get_record_option_impl(record$values, "diagonal")
  if (is.null(diag) && !length(popts)) {
    abort(c("Record has no initial estimates.", record$format()), nmrec_error())
  } else if (is.null(diag)) {
    subtype <- "implicit"
    size <- sum(purrr::map_int(popts, param_x))
  } else {
    n_diag <- matrix_paren_value(diag$value)
    if (identical(n_diag, -1L)) {
      abort(
        c("Failed to parse diagonal (N).", record$format()),
        nmrec_error()
      )
    }
    size <- n_diag

    if (length(popts)) {
      subtype <- "plain"
    } else {
      subtype <- "no_inits"
    }
  }

  return(list(
    type = "diagonal",
    subtype = subtype,
    size = size,
    popts = popts
  ))
}

matrix_classify_block <- function(record, prev) {
  n_block <- matrix_block_n(record)
  n_same <- matrix_same_n(record)
  popts <- param_options(record)

  if (is.null(n_block)) {
    if (is.null(n_same)) {
      abort(
        c(
          "BLOCK must define (N) value when SAME is not used.",
          record$format()
        ),
        nmrec_error()
      )
    }
    n_block <- prev[["size"]]
  }

  if (is.null(n_same)) {
    if (length(popts)) {
      if (matrix_is_vpair(popts)) {
        subtype <- "vpair"
      } else {
        subtype <- "plain"
      }
    } else {
      subtype <- "no_inits"
    }
    size <- n_block
  } else {
    if (is.null(prev) || !identical(prev[["type"]], "block")) {
      abort(
        c(
          "SAME cannot be used if there is not a previous BLOCK.",
          record$format()
        ),
        nmrec_error()
      )
    }

    if (length(popts)) {
      abort(
        c("SAME cannot include explicit initial estimates.", record$format()),
        nmrec_error()
      )
    }

    subtype <- "same"
    size <- n_block * n_same
  }

  return(list(
    type = "block",
    subtype = subtype,
    size = size,
    popts = popts
  ))
}

#' Abort if declared size doesn't match number of initial estimates
#'
#' For example, BLOCK(3) must have six values, and DIAGONAL(3) must have three
#' values.
#' @noRd
matrix_verify_size <- function(info, record) {
  # This is concerned with `info` (what will become a $details element);
  # `record` is passed for the purpose of giving a more informative error.
  if (identical(info[["subtype"]], "plain")) {
    size <- info[["size"]]
    if (identical(info[["type"]], "diagonal")) {
      n_expect <- size
    } else {
      n_expect <- matrix_ltri_size(size)
    }

    popts <- info[["popts"]]
    # In most cases, the number of initial estimates is equal to the number of
    # parameter options in the record. Tally the init options _within_ to
    # account for BLOCK values like (v1 v2 v3)x2.
    opt_inits <- purrr::map_int(
      popts,
      function(o) sum(purrr::map_lgl(o$values, param_value_is_init))
    )
    x <- purrr::map_int(popts, param_x)

    # (v1 v2 v3)x2 (v4)x3 v5 => 10
    n_inits <- sum(opt_inits * x)
    if (n_inits != n_expect) {
      abort(
        c(
          sprintf("Expected %d initial estimates, got %d.", n_expect, n_inits),
          record$format()
        ),
        nmrec_error()
      )
    }
  }
}

#' Extract N (as integer) from BLOCK(N)
#'
#' For bare BLOCK, return NULL.
#' @noRd
matrix_block_n <- function(record) {
  block <- get_record_option_impl(record$values, "block")
  if (inherits(block, "nmrec_option_value")) {
    n <- matrix_paren_value(block$value)
    if (identical(n, -1L)) {
      abort(
        c("Failed to parse block (n) value.", record$format()),
        nmrec_error()
      )
    }
  } else {
    n <- NULL
  }

  return(n)
}

#' Extract N (as integer) from SAME(N)
#'
#' For bare SAME, return the default value (1).
#' @noRd
matrix_same_n <- function(record) {
  same <- get_record_option_impl(record$values, "same")
  if (is.null(same)) {
    return(NULL)
  }

  if (inherits(same, "nmrec_option_value")) {
    n <- matrix_paren_value(same$value)
    if (identical(n, -1L)) {
      abort(
        c("Failed to parse same (n) value.", record$format()),
        nmrec_error()
      )
    }
  } else {
    n <- 1L
  }

  return(n)
}

matrix_paren_value <- function(s) {
  matches <- unlist(regmatches(s, regexec("^ *\\( *([0-9]+) *\\) *$", s)))
  if (length(matches) != 2) {
    return(-1L)
  }
  return(strtoi(matches[2], base = 10))
}

#' Do the parameter options represent VALUES(diag,odiag) pair?
#' @noRd
matrix_is_vpair <- function(popts) {
  length(popts) == 1 &&
    purrr::some(popts[[1]]$values, function(v) {
      inherits(v, "nmrec_option") && v[["name"]] == "values"
    })
}

#' Return environment that maps the ltri index to associated option
#'
#' @param details Details about the records (in form created by
#'   `matrix_classify()`).
#' @param size Size of the square matrix (e.g., 3 for a 3x3 matrix).
#'
#' @noRd
matrix_create_map <- function(details, size) {
  ltri_idxs <- matrix_ltri_indices(size)
  map <- new.env(parent = emptyenv(), size = size)
  ltri_idx <- 1L
  for (ridx in seq_along(details)) {
    info <- details[[ridx]]
    popts <- info[["popts"]]
    type <- info[["type"]]
    subtype <- info[["subtype"]]

    if (identical(type, "diagonal")) {
      if (identical(subtype, "plain") || identical(subtype, "implicit")) {
        idxs_sub <- matrix_sub_diag(ltri_idxs, ltri_idx, info[["size"]])
        map_insert(map, popts, idxs_sub, ridx)
      }
    } else if (identical(type, "block")) {
      idxs_sub <- matrix_sub_ltri(ltri_idxs, ltri_idx, info[["size"]])
      if (identical(subtype, "plain")) {
        map_insert(map, popts, idxs_sub, ridx)
      } else if (identical(subtype, "vpair")) {
        if (length(popts) != 1) {
          # This should be unreachable given parse_ctl() checks.
          bug(paste("vpair has more than one option:", deparse_string(popts)))
        }
        popt <- popts[[1]]
        # Special case: Every other spot inserts on item linking the ltri index
        # to an option. The VALUES(diag,odiag) pair covers two positions, pN and
        # p(N+1), so here we insert two items.
        key <- param_format_key(idxs_sub[[1]])
        map[[key]] <- list(
          opt = get_record_option_impl(popt$values, "diag"),
          popt_index = 1L,
          record_index = ridx
        )
        # NM-TRAN allows a BLOCK(1) VALUES(); odiag is just ignored.
        if (length(idxs_sub) > 1) {
          key <- param_format_key(idxs_sub[[2]])
          map[[key]] <- list(
            opt = get_record_option_impl(popt$values, "odiag"),
            popt_index = 1L,
            record_index = ridx
          )
        }
      }
    } else {
      bug(paste("Unrecognized block type:", deparse_string(type)))
    }
    ltri_idx <- ltri_idx + info[["size"]]
  }
  return(map)
}

map_insert <- function(map, popts, idxs, ridx) {
  idx <- 1L # idxs[idx] => ltri index for full matrix
  for (popt_idx in seq_along(popts)) {
    popt <- popts[[popt_idx]]
    n_init <- 0L
    for (v in popt$values) {
      if (param_value_is_init(v)) {
        n_init <- n_init + 1L
        key <- param_format_key(idxs[[idx + n_init - 1L]])
        map[[key]] <- list(opt = v, popt_index = popt_idx, record_index = ridx)
      }
    }
    idx <- idx + n_init * param_x(popt)
  }
}

#' Return all options for initial estimates
#' @noRd
param_options <- function(record) {
  name <- record[["name"]]
  purrr::keep(record$get_options(), function(o) {
    inherits(o, "nmrec_option_nested") && identical(o[["name"]], name)
  })
}

param_value_is_init <- function(v) {
  inherits(v, "nmrec_option") && identical(v[["name"]], "init")
}

param_x <- function(popt) {
  xopt <- get_record_option_impl(popt$values, "x")
  if (is.null(xopt)) {
    return(1L)
  }

  x <- strtoi(xopt$value, base = 10)
  if (is.na(x)) {
    # This should be unreachable given parse_ctl() checks.
    bug(c("Failed to parse xN value to integer.", xopt$format()))
  }

  return(x)
}

param_format_key <- function(i) {
  return(paste0("p", as.character(i)))
}
