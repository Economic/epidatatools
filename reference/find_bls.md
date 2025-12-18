# Find BLS Series by Search String

Searches the [BLS Data Finder](https://data.bls.gov/dataQuery/search)
for series matching a search string.

Searches the [BLS Data Finder](https://data.bls.gov/dataQuery/search)
for series matching a search string.

## Usage

``` r
find_bls(
  search_string,
  max_results = 20,
  metadata = FALSE,
  survey = NULL,
  seasonality = NULL
)

find_bls(
  search_string,
  max_results = 20,
  metadata = FALSE,
  survey = NULL,
  seasonality = NULL
)
```

## Arguments

- search_string:

  Character string to search for in series titles

- max_results:

  Maximum number of results to return (default: 20)

- metadata:

  Logical flag to retrieve catalog metadata (default: FALSE). When
  FALSE, only series_id and series_title are returned for faster
  performance.

- survey:

  Character string specifying a BLS survey code to restrict the search
  (default: NULL). When NULL, searches across all surveys. See [BLS Data
  Finder](https://data.bls.gov/dataQuery/find?removeAll=1) for available
  survey codes (e.g., "cw" for CPI Urban Wage Earners, "ln" for Labor
  Force Statistics, "ce" for Current Employment Statistics).

- seasonality:

  Character string to filter by seasonal adjustment (default: NULL).
  When NULL, no filter is applied. Use "SA" to restrict to seasonally
  adjusted series, or "NSA" to restrict to not seasonally adjusted
  series.

## Value

A tibble with columns series_id, series_title, and optionally metadata
(a list column containing catalog metadata as one-row tibbles when
metadata = TRUE)

A tibble with columns series_id, series_title, and optionally metadata
(a list column containing catalog metadata as one-row tibbles when
metadata = TRUE)

## Examples

``` r
find_bls("unemployment rate")
#> # A tibble: 20 × 2
#>    series_id            series_title                                            
#>    <chr>                <chr>                                                   
#>  1 LNS14000000          (Seas) Unemployment Rate                                
#>  2 LNU04000000          (Unadj) Unemployment Rate                               
#>  3 LNS14000006          (Seas) Unemployment Rate - Black or African American    
#>  4 LAUCN290950000000003 Unemployment Rate: Jackson County, MO (U)               
#>  5 LASST060000000000003 Unemployment Rate: California (S)                       
#>  6 LASST010000000000003 Unemployment Rate: Alabama (S)                          
#>  7 LAUST010000000000003 Unemployment Rate: Alabama (U)                          
#>  8 LAUST060000000000003 Unemployment Rate: California (U)                       
#>  9 LAUST480000000000003 Unemployment Rate: Texas (U)                            
#> 10 LAUMT363562000000003 Unemployment Rate: New York-Newark-Jersey City, NY-NJ M…
#> 11 LASST260000000000003 Unemployment Rate: Michigan (S)                         
#> 12 LAUMT482642000000003 Unemployment Rate: Houston-Pasadena-The Woodlands, TX M…
#> 13 LASST120000000000003 Unemployment Rate: Florida (S)                          
#> 14 LASST130000000000003 Unemployment Rate: Georgia (S)                          
#> 15 LASST480000000000003 Unemployment Rate: Texas (S)                            
#> 16 LASST080000000000003 Unemployment Rate: Colorado (S)                         
#> 17 LAUMT481910000000003 Unemployment Rate: Dallas-Fort Worth-Arlington, TX Metr…
#> 18 LASST170000000000003 Unemployment Rate: Illinois (S)                         
#> 19 LASST040000000000003 Unemployment Rate: Arizona (S)                          
#> 20 LAUMT171698000000003 Unemployment Rate: Chicago-Naperville-Elgin, IL-IN Metr…

find_bls("wage", survey = "cw")
#> # A tibble: 20 × 2
#>    series_id      series_title                                                  
#>    <chr>          <chr>                                                         
#>  1 CWUR0000SA0    "All items in U.S. city average, urban wage earners and cleri…
#>  2 CWUR0000SA0L1E "All items less food and energy in U.S. city average, urban w…
#>  3 CWUR0000AA0    "All items - old base in U.S. city average, urban wage earner…
#>  4 CWURS49DSA0    "All items in Seattle-Tacoma-Bellevue, WA, urban wage earners…
#>  5 CWURS49ASA0    "All items in Los Angeles-Long Beach-Anaheim, CA, urban wage …
#>  6 CWURS49BSA0    "All items in San Francisco-Oakland-Hayward, CA, urban wage e…
#>  7 CWSR0000SA0    "All items in U.S. city average, urban wage earners and cleri…
#>  8 CWUR0400SA0    "All items in West urban, urban wage earners and clerical wor…
#>  9 CWURS12ASA0    "All items in New York-Newark-Jersey City, NY-NJ-PA, urban wa…
#> 10 CWUR0000SAM    "Medical care in U.S. city average, urban wage earners and cl…
#> 11 CWURS23ASA0    "All items in Chicago-Naperville-Elgin, IL-IN-WI, urban wage …
#> 12 CWUR0000SAH1   "Shelter in U.S. city average, urban wage earners and clerica…
#> 13 CWUR0000SA0E   "Energy in U.S. city average, urban wage earners and clerical…
#> 14 CWUR0300SA0    "All items in South urban, urban wage earners and clerical wo…
#> 15 CWUR0000SETB01 "Gasoline (all types) in U.S. city average, urban wage earner…
#> 16 CWUR0000SEMC01 "Physicians\\' services in U.S. city average, urban wage earn…
#> 17 CWUR0000SEMD01 "Hospital services in U.S. city average, urban wage earners a…
#> 18 CWUR0200SA0    "All items in Midwest urban, urban wage earners and clerical …
#> 19 CWUR0000SEMF01 "Prescription drugs in U.S. city average, urban wage earners …
#> 20 CWURS49ESA0    "All items in San Diego-Carlsbad, CA, urban wage earners and …
if (FALSE) {
find_bls("unemployment rate")

find_bls("wage", survey = "cw")
}
```
