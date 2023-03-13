
Based on guides IV and VIII


Rules
-----

Notes from NM-TRAN guide III.III.A ("Record Syntax - General"):

 * Record and option names may be upper or lower case.

 * The order of records or options doesn't matter except where noted
   in documentation for specific a record or option.

 * Record names and options from before NONMEM 7 can be abbreviated to
   3 letter prefix.

 * Most records of same name are collapsed.  Exceptions are
   estimation, table, and scatterplot.

 * `&` can indicate continuation, with semicolon escaping.

 * `=` for an option may be surrounded by spaces.

 * `=` can be omitted for option.

 * Option names can be separated by spaces or commas

 * File names are limited to single line and cannot have embedded
   spaces.  Must be quoted if it contains any of `,;()` or starts with
   `=` or collides with an option name.

Notes from testing:

 * It is invalid to have space between `$` and name.

 * Tab before record name and between options is treated the same as a
   regular space.

 * If separating options by commas, it's invalid to have multiple
   commas.
