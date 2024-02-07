# nmrec 0.4.0

## New features

* New functions `extract_theta()`, `extract_omega()`, and
  `extract_sigma()` provide an interface for extracting the initial
  estimates of `$THETA`, `$OMEGA`, and `$SIGMA` records into numeric
  objects.  (#30, #34)


# nmrec 0.3.0

## New features and changes

* New functions `set_theta()`, `set_omega()`, and `set_sigma()`
  provide an interface for setting the initial estimates of `$THETA`,
  `$OMEGA`, and `$SIGMA` records.  (#18)

* The `VALUES(diag,odiag)` option in `$OMEGA` and `$SIGMA` records
  used to be parsed into an option with the value "(diag,odiag)".  It
  is now parsed into a nested option that includes `diag` and `odiag`
  options, enabling `diag` and `odiag` to be more easily accessed and
  set.  (#18)

* `parse_ctl()` now checks whether any of the input lines have newline
  characters (which is invalid) and aborts.  (#21)

## Bug fixes

* `$THTA` was not recognized as an alias for `$THETA`.  (#17)

* For the `$THETA` form "(low,,up)", the parser returned an option
  with `up` as the `init` value.  (#18)

* Users may insert a string for a record in the `records` list of
  `nmrec_ctl_records` objects, but `select_records()` crashed on
  string values.  It now skips strings.  (#22)

# nmrec 0.2.0

## New features

* New convenience function `set_record_option()` provides an interface
  for adding and modifying standard options.  (#5)

* Parsing within `$PROBLEM` records is now supported. (#10)

## Bug fixes

* `nmrec` was not compatible with R versions before 4.1 due to the use
   of `gregexec()`.  (#11)


# nmrec 0.1.0

Initial release (#1)
