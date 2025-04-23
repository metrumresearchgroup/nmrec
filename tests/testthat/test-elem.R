test_that("split_to_elements() works", {
  cases <- list(
    list(
      input = "",
      want = list(elem_linebreak())
    ),
    list(
      input = c("", "foo", ""),
      want = list(elem_linebreak(), "foo", elem_linebreak(), elem_linebreak())
    ),
    list(
      input = "    ",
      want = list(elem_whitespace("    "), elem_linebreak())
    ),
    list(
      input = "    ",
      want = list(elem_whitespace("    "), elem_linebreak())
    ),
    list(
      input = "foo",
      want = list("foo", elem_linebreak())
    ),
    list(
      input = c("foo", "bar"),
      want = list("foo", elem_linebreak(), "bar", elem_linebreak())
    ),
    list(
      input = c("", "  foo bar"),
      want = list(
        elem_linebreak(),
        elem_whitespace("  "),
        "foo",
        elem_whitespace(" "),
        "bar",
        elem_linebreak()
      )
    ),
    list(
      input = c(" foo  bar", "baz\t "),
      want = list(
        elem_whitespace(" "),
        "foo",
        elem_whitespace("  "),
        "bar",
        elem_linebreak(),
        "baz",
        elem_whitespace("\t "),
        elem_linebreak()
      )
    ),
    list(
      input = c("; foo", "bar ; baz"),
      want = list(
        elem_comment("; foo"),
        elem_linebreak(),
        "bar",
        elem_whitespace(" "),
        elem_semicolon(),
        elem_whitespace(" "),
        "baz",
        elem_linebreak()
      )
    ),
    list(
      input = c(" \t ; foo  ", "bar ; baz"),
      want = list(
        elem_whitespace(" \t "),
        elem_semicolon(),
        elem_whitespace(" "),
        "foo",
        elem_whitespace("  "),
        elem_linebreak(),
        "bar",
        elem_whitespace(" "),
        elem_semicolon(),
        elem_whitespace(" "),
        "baz",
        elem_linebreak()
      )
    ),
    list(
      input = c("fo,o\t&;", "bar=baz &", "\"'", "(abc)"),
      want = list(
        "fo",
        elem_comma(),
        "o",
        elem_whitespace("\t"),
        elem_ampersand(),
        elem_semicolon(),
        elem_linebreak(),
        "bar",
        elem_equal_sign(),
        "baz",
        elem_whitespace(" "),
        elem_ampersand(),
        elem_linebreak(),
        elem_quote_double(),
        elem_quote_single(),
        elem_linebreak(),
        elem_paren_open(),
        "abc",
        elem_paren_close(),
        elem_linebreak()
      )
    )
  )

  for (case in cases) {
    res <- split_to_elements(case$input)
    expect_identical(res, case$want)
    # Inputs and results match when rendered as string.
    expect_identical(
      paste0(paste0(unlist(res), collapse = "")),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})

test_that("elem_get_class() errors on unknown class", {
  expect_error(elem_get_class("foobert"), class = "nmrec_dev_error")
})
