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
if (FALSE) { # interactive()
find_bls("unemployment rate", survey = "ln", seasonality = "SA")

find_bls("registered nurses wages")

find_bls("all items", survey = "cu", titles_only = TRUE)
}
```
