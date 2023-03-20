
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
   * abb
   * abbre
   * abbrev
   * abbrevi
   * abbrevia
   * abbreviat
   * abbreviate

 * aes (parse: no)

 * aesinit (parse: no)

   aliases:
   - aes0

   abbreviations to check:
   * aesi
   * aesin
   * aesini

 * anneal (parse: no)

   abbreviations to check:
   * anne
   * annea

 * bind (parse: no)

   abbreviations to check:
   * bin

 * chain (parse: yes)

   abbreviations to check:
   * cha
   * chai

   check substring abbreviation for all options

 * contr (parse: no)

   abbreviations to check:
   * con
   * cont

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

 * data (parse: partial)

   notes:
   * filename must be first argument and must be on first line
   * filename may be separated from next argument with comma
   * `*` is for when there is more than one problem (which at least
     initially won't be supported)

   aliases:
   * infile

 * default (parse: no)

   aliases:
   * defaults

   abbreviations to check:
   * def
   * defa
   * defau
   * defaul

 * des (parse: no)

 * design (parse: no)

   aliases:
   * optdesign
   * opt

   abbreviations:
   * desi (noted in VIII)

   abbreviations to check:
   * desig
   * optd
   * optde
   * optdes
   * optdesi
   * optdesig

 * error (parse: no)

   abbreviations to check:
   * err
   * erro

 * estimation (parse: yes)

   aliases:
   * estm
   * estimate

   abbreviations:
   * est

   abbreviations to check:
   * esti
   * estim
   * estima
   * estimat
   * estimati
   * estimatio

   check substring abbreviation for all options

 * etas (parse: no)

   abbreviations to check:
   * eta

 * format (parse: no)

   aliases to check:
   * fmtn (probably not, but V appendix 3 lists this name)

   abbreviations to check:
   * for
   * form
   * forma

 * index (parse: no)

   aliases:
   * indxs
   * indexes

   abbreviations to check:
   * ind
   * inde
   * indx
   * indexe

 * infn (parse: no)

   abbreviations to check:
   * inf

 * input (parse: no)

   abbreviations to check:
   * inp
   * inpu

 * level (parse: no)

   abbreviations to check:
   * lev
   * leve

 * mix (parse: no)

 * model (parse: no)

   abbreviations to check:
   * mod
   * mode

 * msfi (parse: no)

   abbreviations to check:
   * msf

 * nonparametric (parse: no)

   abbreviations to check:
   * non
   * nonp
   * nonpa
   * nonpar
   * nonpara
   * nonparam
   * nonparame
   * nonparamet
   * nonparametr
   * nonparametri

 * olkjdf (parse: no)

   abbreviations to check:
   * olk
   * olkj
   * olkjd

 * omega (parse: yes)

   abbreviations to check:
   * ome
   * omeg

   option aliases:
   * sd

   check substring abbreviation for all options

 * omegap (parse: no)

 * omegapd (parse: no)

 * omit (parse: no)

   abbreviations to check:
   * omi

 * ovarf (parse: no)

   abbreviations to check:
   * ova
   * ovar

 * phis (parse: no)

   abbreviations to check:
   * phi

 * pk (parse: no)

 * pred (parse: no)

   abbreviations to check:
   * pre

 * prior (parse: no)

   abbreviations to check:
   * pri
   * prio

   Note: May need to parse at least subroutine to interpret theta,
   omega, sigma.

 * problem (parse: no)

   abbreviations:
   * prob

   abbreviations to check:
   * pro
   * probl
   * proble

 * rcov (parse: no)

   abbreviations to check:
   * rco (probably can't work due to rcovi)

 * rcovi (parse: no)

 * scatterplot (parse: no)

   aliases:
   * scatters
   * scattergrams

   abbreviations:
   * scat

   abbreviations to check:
   * sca
   * scatt
   * scatte
   * scatter
   * scatterp
   * scatterpl
   * scatterplo

 * sigma (parse: yes)

   abbreviations to check:
   * sig
   * sigm

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
   * sim
   * simu
   * simul
   * simula
   * simulat
   * simulati
   * simulatio

 * sizes (parse: no)

   abbreviations to check:
   * siz
   * size

 * slkjdf (parse: no)

   abbreviations to check:
   * slk
   * slkj
   * slkjd

 * subroutines (parse: no)

   aliases:
   * subs

   abbreviations:
   * sub

   abbreviations to check:
   * subr
   * subro
   * subrou
   * subrout
   * subrouti
   * subroutin
   * subroutine

 * super (parse: no)

   abbreviations to check:
   * sup
   * supe

 * svarf (parse: no)

   abbreviations to check:
   * sva
   * svar

 * table (parse: yes)

   abbreviations:
   * tab

   abbreviations to check:
   * tabl

   check substring abbreviation for all options

 * theta (parse: yes)

   abbreviations to check:
   * the
   * thet

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
   * ttd

 * warnings (parse: no)

   abbreviations:
   * warning

   abbreviations to check:
   * war
   * warn
   * warni
   * warnin
