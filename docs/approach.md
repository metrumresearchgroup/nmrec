
 * Implement according to IV and VIII specification.  If some detail
   from the specification is not supported, an error should be raised.

 * Recognize all documented aliases and abbreviations for record types
   and option names.

 * Parsing happens at two main levels: parsing of records from a
   stream and parsing of content within a record.  For the latter,
   incrementally add support for parsing different record types as
   needed.

 * Wait to parse at record level until needed by requested action
   (e.g., getting or modifying an option).

 * Where it can be done reliably, parsing only select options is okay.
   This is mostly relevant for 'data' records, where the first option
   must be the file name.

 * Overwriting a control stream on disk should only change records
   that were modified.  When modifying a record, it's okay to change
   other inconsequential details on that line (e.g., number of spaces
   between options).

 * It is _not_ a goal to catch syntax errors or other invalid control
   streams when parsing.

 * On modification, it is _not_ a goal to validate the new content,
   aside from checking that it does not lead to a syntax error.
