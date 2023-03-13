
Objects
-------

 * nmrec_ctl_records

   * frontmatter (character vector): lines before first record

   * records (list of nmrec_name_record)

 * nmrec_name_record

   * nmrec_name_record_unparsed

     * name (string): normalized record name (e.g., "table")

     * name_raw (string): record name as specified in source (e.g.,
       "TAB")

     * lines (character vector): unparsed lines of record.  This
       includes any trailing comments.

   * nmrec_name_record_data

     * name_raw (string): data record as specified in source

     * filename (string): data file, unquoted if necessary

     * rest (character vector): remaining, unparsed part of record

   * nmrec_name_record_{chain,estimation,table}

     * name_raw (string): record as specified in source

     * for each option, option name -> nmrec_option_flag or
       nmrec_option_flag

     * template (list of string or nmrec_break or nmrec_comment): list
       that specifies how to write the option.

       For example, for an "estimation" object

           list("method" "interaction" "maxevals"
                nmrec_comment
                nmrec_break
                "print" "noabort")

       might be translated to

           $EST METHOD=1 INTERACTION MAXEVAL=9999 ; a comment
             PRINT=5 NOABORT POSTHOC

 * nmrec_record_element

   * nmrec_option_flag

     * name (string): normalized option name

     * name_raw (string): option name as specified in source

   * nmrec_option_value

     * name (string): normalized option name

     * name_raw (string): option name as specified in source

     * value (string)

   * nmrec_break: newline indicator

     * prefix (string): leading white space for next line

   * nmrec_comment

     * value (string)


Functions
---------

Reading and writing control stream:

 * read_ctl(path) -> nmrec_ctl_records
 * write_ctl(nmrec_ctl_records, path)

Getting and setting records for given (normalized) name (e.g.,
"TABLE"):

 * get_name_records(nmrec_ctl_records, name) -> list of nmrec_name_record
 * set_name_records(nmrec_ctl_records, name, list of nmrec_name_record) -> nmrec_ctl_records

 or perhaps:

 * name_records(nmrec_ctl_records, name) -> nmrec_name_records
 * name_records(nmrec_ctl_records, name) <- nmrec_name_records

Get or set option for given record:

 * get_rec_option(nmrec_name_record, option) -> value
 * set_rec_option(nmrec_name_record, option, value) -> nmrec_name_record

 or perhaps:

 * rec_option(nmrec_name_record, option) -> value
 * rec_option(nmrec_name_record, option) <- value

R6 will likely end up being a more natural fit here, in which case the
getting and setting would be absorbed into those (mutable) objects.

additional specialized helpers (e.g., for theta, omega, and sigma)
