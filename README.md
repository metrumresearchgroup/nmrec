
[![Build Status](https://github-drone.metrumrg.com/api/badges/metrumresearchgroup/nmrec/status.svg)](https://github-drone.metrumrg.com/metrumresearchgroup/nmrec)

## Overview

`nmrec` is an R package for reading records from a NONMEM control
stream, parsing and modifying records of interest, and writing the
result.

Its creation was prompted by several features in [bbr] and [bbr.bayes]
that require modifying a NONMEM control stream.  The purpose of
`nmrec` is to free these package from worrying about many details of
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

At this point, `nmrec` is focused on parsing records into objects that
are very closely tied to the form in the control stream and that don't
lose information.  In the future (and as needed), the plan is to add
more convenience wrappers for working with these objects and helpers
for getting things in and out of higher-level forms.


## Installation

`nmrec` is not yet available from a package archive.  To install the
latest development version from GitHub, you can use `remotes`:

```
# install.packages("remotes")
remotes::install_github("metrumresearchgroup/nmrec")
```


[bbr.bayes]: https://metrumresearchgroup.github.io/bbr.bayes
[bbr]: https://metrumresearchgroup.github.io/bbr
