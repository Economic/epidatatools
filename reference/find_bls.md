# Find BLS Series by Search String

Searches a catalog of every series published by the Bureau of Labor
Statistics (8.3 million series from 33 BLS programs) for titles and
surveys matching a search string. The catalog is built weekly from the
BLS flat files by [bls-index](https://github.com/Economic/bls-index).
Use the series IDs with
[`get_bls()`](https://economic.github.io/epidatatools/reference/get_bls.md).

## Usage

``` r
find_bls(
  search_string,
  survey = NULL,
  seasonality = NULL,
  limit = 1000,
  titles_only = FALSE,
  refresh = FALSE
)
```

## Arguments

- search_string:

  Character string of words to search for.

- survey:

  Character vector of BLS program codes to search within, such as `"ln"`
  (Current Population Survey) or `c("la", "ln")`, ignoring case. The
  default, `NULL`, searches all programs. An unknown code is an error
  that lists the valid codes.

- seasonality:

  Character vector of seasonal adjustment values to keep: one or more of
  `"SA"`, `"NSA"` and `"unknown"`, ignoring case. The default, `NULL`,
  keeps all.

- limit:

  Maximum number of rows to return (default: 1000). Use `Inf` for all
  matches. When rows are cut, a message gives the total and the matches
  per survey.

- titles_only:

  Logical. If `TRUE`, every word must match the series title; survey
  names and aliases are ignored. Useful with `survey`, as in
  `find_bls("all items", survey = "cu", titles_only = TRUE)`.

- refresh:

  Logical. If `TRUE`, check for a newer catalog now.

## Value

A tibble with columns `series_id`, `series_title`, `program` (the BLS
program code), `survey_name`, `seasonal` (`"SA"`, `"NSA"` or
`"unknown"`), `begin_year`, `end_year` and `end_period`.

## Searching

Every word in `search_string` must match, ignoring case, in the series
title, the survey name or a survey alias (such as "CPI" or "JOLTS").
Words match by the part they share with their English stem, so "nurses"
also finds "nursing" and "industries" also finds "industry". When that
part is shorter than four letters, the word must match a whole word, so
"CES" does not match "services". A short word ending in "y" or "ies"
also matches its other form, so "city" finds "cities".

Results are sorted by the number of words that match the title or a
survey alias, then by title length (shortest first), then by title and
series ID. Short titles tend to be the general series. Narrow a search
with more words or `survey`.

## The catalog

The first search downloads the catalog (about 33 MB) into
`tools::R_user_dir("epidatatools", "cache")` and verifies its checksum.
Later searches use that copy, and check for a newer catalog at most once
every 7 days. If the check fails, for example offline, the cached
catalog is used. A catalog built more than 10 days ago is reported as
possibly out of date.

Settings, for testing or mirrors:

- `options(bls_index.manifest_url = ...)`, or else the
  `BLS_INDEX_MANIFEST_URL` environment variable: the catalog manifest to
  read

- `options(bls_index.cache_dir = ...)`: where to keep the catalog

Searching needs the arrow package, which `find_bls()` offers to install.

## Examples

``` r
find_bls("unemployment rate", survey = "ln", seasonality = "SA")
#> # A tibble: 184 × 8
#>    series_id    series_title    program survey_name seasonal begin_year end_year
#>    <chr>        <chr>           <chr>   <chr>       <chr>         <int>    <int>
#>  1 LNS14000000  (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#>  2 LNS14000000Q (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#>  3 LNS14000001  (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#>  4 LNS14000001Q (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#>  5 LNS14032183  (Seas) Unemplo… ln      Labor Forc… SA             2003     2026
#>  6 LNS14032183Q (Seas) Unemplo… ln      Labor Forc… SA             2010     2026
#>  7 LNS14000003  (Seas) Unemplo… ln      Labor Forc… SA             1954     2026
#>  8 LNS14000003Q (Seas) Unemplo… ln      Labor Forc… SA             1954     2026
#>  9 LNS14000002  (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#> 10 LNS14000002Q (Seas) Unemplo… ln      Labor Forc… SA             1948     2026
#> # ℹ 174 more rows
#> # ℹ 1 more variable: end_period <chr>

find_bls("registered nurses wages")
#> 18,897 matches; showing the first 1,000. Narrow the search, or use `limit = Inf`.
#> By survey: oe 12,674, wm 6,206, le 10, cm 6, ep 1.
#> # A tibble: 1,000 × 8
#>    series_id       series_title program survey_name seasonal begin_year end_year
#>    <chr>           <chr>        <chr>   <chr>       <chr>         <int>    <int>
#>  1 EPU291141TE1200 Registered … ep      Employment… NSA            2025     2025
#>  2 WMU39000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  3 WMU49000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  4 WMU23000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  5 WMU02000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  6 WMU15000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  7 WMU32000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  8 WMU41000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#>  9 WMU19000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#> 10 WMU39000001020… Average hou… wm      Modeled Wa… NSA            2023     2023
#> # ℹ 990 more rows
#> # ℹ 1 more variable: end_period <chr>

find_bls("all items", survey = "cu", titles_only = TRUE)
#> # A tibble: 702 × 8
#>    series_id   series_title     program survey_name seasonal begin_year end_year
#>    <chr>       <chr>            <chr>   <chr>       <chr>         <int>    <int>
#>  1 CUUR0490SA0 All items in Pa… cu      Consumer P… NSA            2017     2026
#>  2 CUUS0490SA0 All items in Pa… cu      Consumer P… NSA            2018     2026
#>  3 CUUR0480SA0 All items in Mo… cu      Consumer P… NSA            2017     2026
#>  4 CUUS0480SA0 All items in Mo… cu      Consumer P… NSA            2018     2026
#>  5 CUUR0400SA0 All items in We… cu      Consumer P… NSA            1966     2026
#>  6 CUUS0400SA0 All items in We… cu      Consumer P… NSA            1984     2026
#>  7 CUUR0110SA0 All items in Ne… cu      Consumer P… NSA            2017     2026
#>  8 CUUS0110SA0 All items in Ne… cu      Consumer P… NSA            2018     2026
#>  9 CUUR0300SA0 All items in So… cu      Consumer P… NSA            1966     2026
#> 10 CUUS0300SA0 All items in So… cu      Consumer P… NSA            1984     2026
#> # ℹ 692 more rows
#> # ℹ 1 more variable: end_period <chr>
```
