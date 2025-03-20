parse_table_record <- function() {
  rp <- record_parser$new("table", private$name_raw, private$lines)

  is_table_list_element <- function(x) {
    lc <- tolower(x)
    if (!is.null(resolve_option(x, table_option_names))) {
      # This can be in list{1,2,3} and collide with option abbreviations.
      return(!(identical(lc, "npd") || identical(lc, "wres")))
    }
    return(identical(lc, "by") || identical(lc, "exclude_by"))
  }

  # Based on NM-TRAN testing, a leading WRES/NPD is treated as the
  # WRESCHOL/NPDTYPE options, not list1 elements, so don't guard this next call.
  process_options(
    rp, table_option_types, table_option_names,
    fail_on_unknown = FALSE,
    value_fns = list("format" = parse_format_option_value)
  )

  saw_list1 <- FALSE
  while (!rp$done()) {
    curr <- tolower(rp$current())
    if (identical(curr, "by")) {
      what <- "by"
    } else if (identical(curr, "exclude_by")) {
      what <- "exclude_by"
    } else {
      what <- "list1"
      if (saw_list1) {
        abort(
          c(
            "list1 repeated in table",
            rp$format()
          ),
          nmrec_error("parse")
        )
      }
      saw_list1 <- TRUE
    }

    opt_idx <- rp$find_next(is_table_list_element)
    if (identical(opt_idx, 0L)) {
      pos <- rp$n_elems
    } else {
      pos <- opt_idx - 1
    }
    if (rp$is(c("whitespace", "linebreak"), pos = pos)) {
      # Although for most cases we just absorb elements (e.g., line breaks and
      # trailing comments) into the list value, handle the common case of one
      # trailing space or newline.
      pos <- pos - 1
    }

    rp$append(option_pos$new(what, value = rp$yank_to(pos)))
    rp$gobble()
    process_options(
      rp, table_option_types, table_option_names,
      fail_on_unknown = FALSE,
      value_fns = list("format" = parse_format_option_value)
    )
  }
  rp$assert_done()

  return(rp$get_values())
}

#' @rdname record
record_table <- R6::R6Class(
  "nmrec_record_table",
  inherit = record
)
record_table$set("private", "parse_fn", parse_table_record)

table_option_types <- list(
  "append" = option_type_flag,
  "clockseed" = option_type_value,
  "conditional" = option_type_flag,
  "esample" = option_type_value,
  "file" = option_type_value,
  "firstlastonly" = option_type_flag,
  "firstonly" = option_type_flag,
  "fixedetas" = option_type_value,
  "format" = option_type_value,
  "forward" = option_type_flag,
  "idformat" = option_type_value,
  "lastonly" = option_type_flag,
  "lformat" = option_type_value,
  "noappend" = option_type_flag,
  "noforward" = option_type_flag,
  "noheader" = option_type_flag,
  "nolabel" = option_type_flag,
  "noprint" = option_type_flag,
  "nosub" = option_type_value,
  "notitle" = option_type_flag,
  "npdtype" = option_type_value,
  "omitted" = option_type_flag,
  "oneheader" = option_type_flag,
  "oneheaderall" = option_type_flag,
  "parafile" = option_type_value,
  "print" = option_type_flag,
  "ranmethod" = option_type_value,
  "rformat" = option_type_value,
  "seed" = option_type_value,
  "unconditional" = option_type_flag,
  "varcalc" = option_type_value,
  "wreschol" = option_type_flag
)

table_option_names <- list2env(
  list(
    "app" = "append",
    "appe" = "append",
    "appen" = "append",
    "append" = "append",
    "clo" = "clockseed",
    "cloc" = "clockseed",
    "clock" = "clockseed",
    "clocks" = "clockseed",
    "clockse" = "clockseed",
    "clocksee" = "clockseed",
    "clockseed" = "clockseed",
    "con" = "conditional",
    "cond" = "conditional",
    "condi" = "conditional",
    "condit" = "conditional",
    "conditi" = "conditional",
    "conditio" = "conditional",
    "condition" = "conditional",
    "conditiona" = "conditional",
    "conditional" = "conditional",
    "esa" = "esample",
    "esam" = "esample",
    "esamp" = "esample",
    "esampl" = "esample",
    "esample" = "esample",
    "fil" = "file",
    "file" = "file",
    "fir" = "firstonly",
    "firs" = "firstonly",
    "first" = "firstonly",
    "firstl" = "firstlastonly",
    "firstla" = "firstlastonly",
    "firstlas" = "firstlastonly",
    "firstlast" = "firstlastonly",
    "firstlasto" = "firstlastonly",
    "firstlaston" = "firstlastonly",
    "firstlastonl" = "firstlastonly",
    "firstlastonly" = "firstlastonly",
    "firsto" = "firstonly",
    "firston" = "firstonly",
    "firstonl" = "firstonly",
    "firstonly" = "firstonly",
    "firstr" = "firstonly",
    "firstre" = "firstonly",
    "firstrec" = "firstonly",
    "firstreco" = "firstonly",
    "firstrecon" = "firstonly",
    "firstreconl" = "firstonly",
    "firstreconly" = "firstonly",
    "firstrecor" = "firstonly",
    "firstrecord" = "firstonly",
    "firstrecordo" = "firstonly",
    "firstrecordon" = "firstonly",
    "firstrecordonl" = "firstonly",
    "firstrecordonly" = "firstonly",
    "fix" = "fixedetas",
    "fixe" = "fixedetas",
    "fixed" = "fixedetas",
    "fixede" = "fixedetas",
    "fixedet" = "fixedetas",
    "fixedeta" = "fixedetas",
    "fixedetas" = "fixedetas",
    "for" = "format",
    "form" = "format",
    "forma" = "format",
    "format" = "format",
    "forw" = "forward",
    "forwa" = "forward",
    "forwar" = "forward",
    "forward" = "forward",
    "idf" = "idformat",
    "idfo" = "idformat",
    "idfor" = "idformat",
    "idform" = "idformat",
    "idforma" = "idformat",
    "idformat" = "idformat",
    "las" = "lastonly",
    "last" = "lastonly",
    "lasto" = "lastonly",
    "laston" = "lastonly",
    "lastonl" = "lastonly",
    "lastonly" = "lastonly",
    "lastr" = "lastonly",
    "lastre" = "lastonly",
    "lastrec" = "lastonly",
    "lastreco" = "lastonly",
    "lastrecon" = "lastonly",
    "lastreconl" = "lastonly",
    "lastreconly" = "lastonly",
    "lastrecor" = "lastonly",
    "lastrecord" = "lastonly",
    "lastrecordo" = "lastonly",
    "lastrecordon" = "lastonly",
    "lastrecordonl" = "lastonly",
    "lastrecordonly" = "lastonly",
    "lfo" = "lformat",
    "lfor" = "lformat",
    "lform" = "lformat",
    "lforma" = "lformat",
    "lformat" = "lformat",
    "noa" = "noappend",
    "noap" = "noappend",
    "noapp" = "noappend",
    "noappe" = "noappend",
    "noappen" = "noappend",
    "noappend" = "noappend",
    "nof" = "noforward",
    "nofo" = "noforward",
    "nofor" = "noforward",
    "noforw" = "noforward",
    "noforwa" = "noforward",
    "noforwar" = "noforward",
    "noforward" = "noforward",
    "noh" = "noheader",
    "nohe" = "noheader",
    "nohea" = "noheader",
    "nohead" = "noheader",
    "noheade" = "noheader",
    "noheader" = "noheader",
    "nol" = "nolabel",
    "nola" = "nolabel",
    "nolab" = "nolabel",
    "nolabe" = "nolabel",
    "nolabel" = "nolabel",
    "nop" = "noprint",
    "nopr" = "noprint",
    "nopri" = "noprint",
    "noprin" = "noprint",
    "noprint" = "noprint",
    "nos" = "nosub",
    "nosu" = "nosub",
    "nosub" = "nosub",
    "not" = "notitle",
    "noti" = "notitle",
    "notit" = "notitle",
    "notitl" = "notitle",
    "notitle" = "notitle",
    "npd" = "npdtype",
    "npdt" = "npdtype",
    "npdty" = "npdtype",
    "npdtyp" = "npdtype",
    "npdtype" = "npdtype",
    # This is listed on VIII, but NM-TRAN (7.5.0) gives "UNKNOWN OPTION", so
    # leave out abbreviations.
    "omitted" = "omitted",
    "one" = "oneheader",
    "oneh" = "oneheader",
    "onehe" = "oneheader",
    "onehea" = "oneheader",
    "onehead" = "oneheader",
    "oneheade" = "oneheader",
    "oneheader" = "oneheader",
    "oneheaderall" = "oneheaderall",
    "oneheaderperfile" = "oneheaderall",
    "par" = "parafile",
    "para" = "parafile",
    "paraf" = "parafile",
    "parafi" = "parafile",
    "parafil" = "parafile",
    "parafile" = "parafile",
    "pri" = "print",
    "prin" = "print",
    "print" = "print",
    "ran" = "ranmethod",
    "ranm" = "ranmethod",
    "ranme" = "ranmethod",
    "ranmet" = "ranmethod",
    "ranmeth" = "ranmethod",
    "ranmetho" = "ranmethod",
    "ranmethod" = "ranmethod",
    "rfo" = "rformat",
    "rfor" = "rformat",
    "rform" = "rformat",
    "rforma" = "rformat",
    "rformat" = "rformat",
    "see" = "seed",
    "seed" = "seed",
    "unc" = "unconditional",
    "unco" = "unconditional",
    "uncon" = "unconditional",
    "uncond" = "unconditional",
    "uncondi" = "unconditional",
    "uncondit" = "unconditional",
    "unconditi" = "unconditional",
    "unconditio" = "unconditional",
    "uncondition" = "unconditional",
    "unconditiona" = "unconditional",
    "unconditional" = "unconditional",
    "var" = "varcalc",
    "varc" = "varcalc",
    "varca" = "varcalc",
    "varcal" = "varcalc",
    "varcalc" = "varcalc",
    "wre" = "wreschol",
    "wres" = "wreschol",
    "wresc" = "wreschol",
    "wresch" = "wreschol",
    "wrescho" = "wreschol",
    "wreschol" = "wreschol"
  ),
  parent = emptyenv()
)
