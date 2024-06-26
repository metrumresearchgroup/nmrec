

# nmrec <a href='https:/metrumresearchgroup.github.io/nmrec'><img src='man/figures/logo.png' align="right" /></a>

<!-- badges: start -->

[![Build Status](https://github.com/metrumresearchgroup/nmrec/actions/workflows/main.yaml/badge.svg)](https://github.com/metrumresearchgroup/nmrec/actions/workflows/main.yaml)
<!-- badges: end -->

## Overview

`nmrec` is an R package for reading records from a NONMEM control
stream, parsing and modifying records of interest, and writing the
result.

Its creation was prompted by several features in [bbr] and [bbr.bayes]
that require modifying a NONMEM control stream.  The purpose of
`nmrec` is to free these packages from worrying about many details of
parsing.  For example, `nmrec` allows grabbing an estimation record
without accounting for the many ways `$ESTIMATION` could be spelled
(`$EST`, `$ESTM`, or `$esti`, to name just a few).

`nmrec` aims to

 * stick as close to the NONMEM user guide specification as possible
   (e.g., support nearly all abbreviations and aliases for record
   names and options) and try to abort loudly when an unsupported case
   is hit

 * support parsing _within_ record types of interest, adding support
   for other record types as needed


## Installation

`nmrec` is available on [MPN] starting with the 2023-09-19 snapshot.

To install the latest development version from GitHub, you can use
`remotes`:

```
# install.packages("remotes")
remotes::install_github("metrumresearchgroup/nmrec")
```

## Documentation

Documentation, including a ["Get Started"][gs] vignette, is available
at <https://metrumresearchgroup.github.io/nmrec>.


[bbr.bayes]: https://metrumresearchgroup.github.io/bbr.bayes
[bbr]: https://metrumresearchgroup.github.io/bbr
[MPN]: https://mpn.metworx.com
[gs]: https://metrumresearchgroup.github.io/nmrec/articles/nmrec
