
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

 * Options and values can be separated across multiple lines in some
   cases (e.g., `SIG` option of `$ESTIMATION`).  Separator (whitespace
   or =) can be on its own line.  However, in other spots this isn't
   permitted (at least `FILE` option of `$TABLE`).

 * It is not valid to use comma after record name.

 * Option values in some cases can be pinned to the option
   name (e.g., `SIG3`, `IGNORE(ID.EQ.2)`).  At the very least, this
   does not seem to be valid of `FILE` values of `$TABLE`.

   Will likely not support the `SIG3` form.

 * Unlike data file name, files fed to NONMEM (e.g. those for `$TABLE`
   entries) cannot use spaces.

 * At least for `IGNORE` list, can start "(" on one line and then
   continue with content on the next.  Errors if you put
   non-whitespace content after "(" and then put ")" on following
   line.

 * There are some options that are stricter about the form they take
   (e.g., the `MISDAT` option of `$DATA` errors if an equal sign isn't
   used as the delimiter).


Records
-------

Initial within-record parsing planned for chain, data, estimation,
omega, sigma, table, and theta.

 * abbreviated (parse: no)

   abbreviations:
   * abbr

   abbreviations to check:
   - [X] abb
   - [X] abbre
   - [X] abbrev
   - [X] abbrevi
   - [X] abbrevia
   - [X] abbreviat
   - [X] abbreviate

 * aes (parse: no)

 * aesinit (parse: no)

   aliases:
   - aes0

   abbreviations to check:
   - [X] aesi
   - [X] aesin
   - [X] aesini

 * anneal (parse: no)

   abbreviations to check:
   - [X] ann
   - [X] anne
   - [X] annea

 * bind (parse: no)

   abbreviations to check:
   - [X] bin

 * chain (parse: yes)

   abbreviations to check:
   - [X] cha
   - [X] chai

   check substring abbreviation for all options

 * contr (parse: no)

   abbreviations to check:
   - [X] con
   - [X] cont

 * covariance (parse: no)

   aliases:
   * covr

   abbreviations:
   * cov

   abbreviations to check:
   * cova
   * covar
   * covari
   * covaria
   * covarian
   * covarianc

 * data (parse: yes)

   notes:
   * filename must be first argument and must be on first line
   * filename may be separated from next argument with comma
   * `*` is for when there is more than one problem (which at least
     initially won't be supported)

   aliases:
   * infile

   abbreviations to check:
   - [X] dat
   - [X] inf
   * [X] infi
   * [X] infil

   option aliases:
   * checkdata

 * default (parse: no)

   aliases:
   * defaults

   abbreviations to check:
   - [X] def
   - [X] defa
   - [X] defau
   - [X] defaul

 * des (parse: no)

 * design (parse: no)

   aliases:
   * optdesign
   * opt

   abbreviations:
   * desi (noted in VIII)

   abbreviations to check:
   - [X] desig
   - [X] optd
   - [X] optde
   - [X] optdes
   - [X] optdesi
   - [X] optdesig

 * error (parse: no)

   abbreviations to check:
   - [X] err
   - [X] erro

 * estimation (parse: yes)

   aliases:
   * estm
   * estimate

   abbreviations:
   * est

   abbreviations to check:
   - [X] esti
   - [X] estim
   - [X] estima
   - [X] estimat
   - [X] estimati
   - [X] estimatio

   check substring abbreviation for all options

 * etas (parse: no)

   abbreviations to check:
   - [X] eta

 * format (parse: no)

   aliases to check:
   - [X] fmtn (V appendix 3 lists this name): no, unknown control
         record error

   abbreviations to check:
   - [X] for
   - [X] form
   - [X] forma

 * index (parse: no)

   aliases:
   * indxs
   * indexes

   abbreviations to check:
   - [X] ind
   - [X] inde
   - [X] indx
   - [X] indexe

 * infn (parse: no)

   abbreviations to check:
   - [X] inf: no, for infile

 * input (parse: no)

   abbreviations to check:
   - [X] inp
   - [X] inpu

 * level (parse: no)

   abbreviations to check:
   - [X] lev
   - [X] leve

 * mix (parse: no)

 * model (parse: no)

   abbreviations to check:
   - [X] mod
   - [X] mode

 * msfi (parse: no)

   abbreviations to check:
   - [X] msf

 * nonparametric (parse: no)

   abbreviations to check:
   - [X] non
   - [X] nonp
   - [X] nonpa
   - [X] nonpar
   - [X] nonpara
   - [X] nonparam
   - [X] nonparame
   - [X] nonparamet
   - [X] nonparametr
   - [X] nonparametri

 * olkjdf (parse: no)

   abbreviations to check:
   - [X] olk
   - [X] olkj
   - [X] olkjd

 * omega (parse: yes)

   abbreviations to check:
   - [ ] ome
   - [ ] omeg

   option aliases:
   * sd

   check substring abbreviation for all options

 * omegap (parse: no)

 * omegapd (parse: no)

 * omit (parse: no)

   abbreviations to check:
   - [X] omi

 * ovarf (parse: no)

   abbreviations to check:
   - [X] ova
   - [X] ovar

 * phis (parse: no)

   abbreviations to check:
   - [X] phi

 * pk (parse: no)

 * pred (parse: no)

   abbreviations to check:
   - [X] pre

 * prior (parse: no)

   abbreviations to check:
   - [X] pri
   - [X] prio

   Note: May need to parse at least subroutine to interpret theta,
   omega, sigma.

 * problem (parse: no)

   abbreviations:
   * prob

   abbreviations to check:
   - [X] pro
   - [X] probl
   - [X] proble

 * rcov (parse: no)

   abbreviations to check:
   - [X] rco

 * rcovi (parse: no)

 * scatterplot (parse: no)

   aliases:
   * scatters
   * scattergrams

   abbreviations:
   * scat

   abbreviations to check:
   - [X] sca
   - [X] scatt
   - [X] scatte
   - [X] scatter
   - [X] scatterp
   - [X] scatterpl
   - [X] scatterplo
   - [X] scatterg
   - [X] scattergr
   - [X] scattergra
   - [X] scattergram

 * sigma (parse: yes)

   abbreviations to check:
   - [X] sig
   - [X] sigm

   option aliases:
   * sd

   check substring abbreviation for all options

 * sigmap (parse: no)

 * sigmapd (parse: no)

 * simulation (parse: no)

   aliases:
   * simulate
   * siml

   abbreviations to check:
   - [X] sim
   - [X] simu
   - [X] simul
   - [X] simula
   - [X] simulat
   - [X] simulati
   - [X] simulatio

 * sizes (parse: no)

   abbreviations to check:
   - [X] siz
   - [X] size

 * slkjdf (parse: no)

   abbreviations to check:
   - [X] slk
   - [X] slkj
   - [X] slkjd

 * subroutines (parse: no)

   aliases:
   * subs

   abbreviations:
   * sub

   abbreviations to check:
   - [X] subr
   - [X] subro
   - [X] subrou
   - [X] subrout
   - [X] subrouti
   - [X] subroutin
   - [X] subroutine

 * super (parse: no)

   abbreviations to check:
   - [X] sup
   - [X] supe

 * svarf (parse: no)

   abbreviations to check:
   - [X] sva
   - [X] svar

 * table (parse: yes)

   abbreviations:
   * tab

   abbreviations to check:
   * tabl

   check substring abbreviation for all options

 * theta (parse: yes)

   abbreviations to check:
   - [X] the
   - [X] thet

   option aliases:
   * numberpts
   * numpoints
   * nmpts

   check substring abbreviation for all options

 * thetai (parse: no)

 * thetap (parse: no)

 * thetapv (parse: no)

 * thetar (parse: no)

 * tol (parse: no)

 * ttdf (parse: no)

   abbreviations to check:
   - [X] ttd

 * warnings (parse: no)

   abbreviations:
   * warning

   abbreviations to check:
   - [X] war
   - [X] warn
   - [X] warni
   - [X] warnin
