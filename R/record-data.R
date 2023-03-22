# TODO: Document parse_* functions somewhere.

parse_data_record <- function() {
  rp <- record_parser$new(
    private$name_raw, private$lines,
    option_types = data_option_types,
    option_names = data_option_names
  )

  prev <- private$previous_rec
  filename <- NULL
  if (is.null(prev)) {
    filename <- rp$elems_yank(fold_quoted = TRUE)

    if (elem_is(filename, c("linebreak", "semicolon"))) {
      abort(
        paste0("$", self$name_raw, " filename option must be on first line"),
        "nmrec_parse_error"
      )
    }

    if (identical(filename, "*")) {
      abort(
        "nmrec does not support filename=* for $DATA records.",
        "nmrec_unsupported"
      )
    }

    rp$options_append(option_pos$new("filename", value = filename))
  } else {
    # Parsing the previous $DATA record is necessary to decide how to parse any
    # subsequent ones.
    prev$parse()
  }

  rp$gobble()

  file_only <- !is.null(filename) ||
    identical(names(prev$options), "filename")
  # (format)
  if (file_only && rp$elems_is("paren_open")) {
    pos <- rp$elems_find_next(~ elem_is(.x, "paren_close"))
    if (identical(pos, 0L)) {
      abort(
        c("Missing closing paren.", paste(rp$elems, collapse = "")),
        "nmrec_parse_error"
      )
    }

    rp$options_append(
      option_pos$new("format", value = rp$elems_yank_to(pos))
    )
  }

  rp$process_options()

  return(list(template = rp$get_template(), options = rp$get_options()))
}

record_data <- R6::R6Class(
  "nmrec_record_data",
  inherit = record
)
record_data$set("private", "parse_fn", parse_data_record)

data_option_types <- list(
  "accept" = option_type_value,
  "blankok" = option_type_flag,
  "checkout" = option_type_flag,
  "ignore" = option_type_value,
  "last20" = option_type_value,
  "lrecl" = option_type_value,
  "misdat" = option_type_value,
  "noopen" = option_type_flag,
  "norewind" = option_type_flag,
  "nowide" = option_type_flag,
  "null" = option_type_value,
  "pred_ignore_data" = option_type_flag,
  "records" = option_type_value,
  "repl" = option_type_value,
  "rewind" = option_type_flag,
  "translate" = option_type_value,
  "wide" = option_type_flag
)

data_option_names <- list2env(
  list(
    "acc" = "accept",
    "acce" = "accept",
    "accep" = "accept",
    "accept" = "accept",
    "blankok" = "blankok",
    # NM-TRAN appears to accept one and two character forms for checkout.
    "c" = "checkout",
    "ch" = "checkout",
    "che" = "checkout",
    "chec" = "checkout",
    "check" = "checkout",
    "checkd" = "checkout",
    "checkda" = "checkout",
    "checkdat" = "checkout",
    "checkdata" = "checkout",
    "checko" = "checkout",
    "checkou" = "checkout",
    "checkout" = "checkout",
    "ign" = "ignore",
    "igno" = "ignore",
    "ignor" = "ignore",
    "ignore" = "ignore",
    "last20" = "last20",
    "lre" = "lrecl",
    "lrec" = "lrecl",
    "lrecl" = "lrecl",
    "mis" = "misdat",
    "misd" = "misdat",
    "misda" = "misdat",
    "misdat" = "misdat",
    "noo" = "noopen",
    "noop" = "noopen",
    "noope" = "noopen",
    "noopen" = "noopen",
    "nor" = "norewind",
    "nore" = "norewind",
    "norew" = "norewind",
    "norewi" = "norewind",
    "norewin" = "norewind",
    "norewind" = "norewind",
    "now" = "nowide",
    "nowi" = "nowide",
    "nowid" = "nowide",
    "nowide" = "nowide",
    "nrec" = "records",
    "nreco" = "records",
    "nrecor" = "records",
    "nrecord" = "records",
    "nrecords" = "records",
    "nrecs" = "records",
    "nul" = "null",
    "null" = "null",
    "pre" = "pred_ignore_data",
    "pred" = "pred_ignore_data",
    "pred_" = "pred_ignore_data",
    "pred_i" = "pred_ignore_data",
    "pred_ig" = "pred_ignore_data",
    "pred_ign" = "pred_ignore_data",
    "pred_igno" = "pred_ignore_data",
    "pred_ignor" = "pred_ignore_data",
    "pred_ignore" = "pred_ignore_data",
    "pred_ignore_" = "pred_ignore_data",
    "pred_ignore_d" = "pred_ignore_data",
    "pred_ignore_da" = "pred_ignore_data",
    "pred_ignore_dat" = "pred_ignore_data",
    "pred_ignore_data" = "pred_ignore_data",
    "rec" = "records",
    "reco" = "records",
    "recor" = "records",
    "record" = "records",
    "records" = "records",
    "recs" = "records",
    "rep" = "repl",
    "repl" = "repl",
    "rew" = "rewind",
    "rewi" = "rewind",
    "rewin" = "rewind",
    "rewind" = "rewind",
    "tra" = "translate",
    "tran" = "translate",
    "trans" = "translate",
    "transl" = "translate",
    "transla" = "translate",
    "translat" = "translate",
    "translate" = "translate",
    "wid" = "wide",
    "wide" = "wide"
  ),
  parent = emptyenv()
)
