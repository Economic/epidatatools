# State Geographic Codes

A dataset containing geographic codes for US states including FIPS
codes, Census codes, and region/division classifications.

## Usage

``` r
state_geocodes
```

## Format

A data frame with 51 rows and 8 variables:

- state_name:

  Full state name

- state_abb:

  Two-letter state abbreviation

- state_fips:

  State FIPS code

- state_census:

  State Census code

- division:

  Census division number (1-9)

- division_name:

  Census division name (e.g., "New England", "Pacific")

- region:

  Census region number (1-4)

- region_name:

  Census region name (Northeast, Midwest, South, West)

## Source

<https://github.com/Economic/state_geocodes>
