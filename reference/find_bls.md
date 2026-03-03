# Find BLS Series by Search String

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

## Examples

``` r
find_bls("unemployment rate")
#> # A tibble: 20 × 2
#>    series_id            series_title                                        
#>    <chr>                <chr>                                               
#>  1 LNS14000000          (Seas) Unemployment Rate                            
#>  2 LNU04000000          (Unadj) Unemployment Rate                           
#>  3 LASST360000000000003 Unemployment Rate: New York (S)                     
#>  4 LASST010000000000003 Unemployment Rate: Alabama (S)                      
#>  5 LNS14000006          (Seas) Unemployment Rate - Black or African American
#>  6 LASST060000000000003 Unemployment Rate: California (S)                   
#>  7 LASST130000000000003 Unemployment Rate: Georgia (S)                      
#>  8 LASST480000000000003 Unemployment Rate: Texas (S)                        
#>  9 LASST020000000000003 Unemployment Rate: Alaska (S)                       
#> 10 LASST040000000000003 Unemployment Rate: Arizona (S)                      
#> 11 LASST230000000000003 Unemployment Rate: Maine (S)                        
#> 12 LASST270000000000003 Unemployment Rate: Minnesota (S)                    
#> 13 LASST120000000000003 Unemployment Rate: Florida (S)                      
#> 14 LASST080000000000003 Unemployment Rate: Colorado (S)                     
#> 15 LASST280000000000003 Unemployment Rate: Mississippi (S)                  
#> 16 LASST050000000000003 Unemployment Rate: Arkansas (S)                     
#> 17 LASST310000000000003 Unemployment Rate: Nebraska (S)                     
#> 18 LASST370000000000003 Unemployment Rate: North Carolina (S)               
#> 19 LASST180000000000003 Unemployment Rate: Indiana (S)                      
#> 20 LASST170000000000003 Unemployment Rate: Illinois (S)                     

find_bls("wage", survey = "cw")
#> # A tibble: 20 × 2
#>    series_id      series_title                                                  
#>    <chr>          <chr>                                                         
#>  1 CWUR0000SA0    "All items in U.S. city average, urban wage earners and cleri…
#>  2 CWUR0000SA0L1E "All items less food and energy in U.S. city average, urban w…
#>  3 CWURS49DSA0    "All items in Seattle-Tacoma-Bellevue, WA, urban wage earners…
#>  4 CWURS49BSA0    "All items in San Francisco-Oakland-Hayward, CA, urban wage e…
#>  5 CWURS49ASA0    "All items in Los Angeles-Long Beach-Anaheim, CA, urban wage …
#>  6 CWUR0000AA0    "All items - old base in U.S. city average, urban wage earner…
#>  7 CWUR0400SA0    "All items in West urban, urban wage earners and clerical wor…
#>  8 CWSR0000SA0    "All items in U.S. city average, urban wage earners and cleri…
#>  9 CWURS23ASA0    "All items in Chicago-Naperville-Elgin, IL-IN-WI, urban wage …
#> 10 CWURS12ASA0    "All items in New York-Newark-Jersey City, NY-NJ-PA, urban wa…
#> 11 CWUR0200SA0    "All items in Midwest urban, urban wage earners and clerical …
#> 12 CWUR0000SAM    "Medical care in U.S. city average, urban wage earners and cl…
#> 13 CWUR0000SAH1   "Shelter in U.S. city average, urban wage earners and clerica…
#> 14 CWUR0300SA0    "All items in South urban, urban wage earners and clerical wo…
#> 15 CWUR0000SETB01 "Gasoline (all types) in U.S. city average, urban wage earner…
#> 16 CWUR0000SA0E   "Energy in U.S. city average, urban wage earners and clerical…
#> 17 CWUS0000SA0    "All items in U.S. city average, urban wage earners and cleri…
#> 18 CWUR0000SEMC01 "Physicians\\' services in U.S. city average, urban wage earn…
#> 19 CWUR0100SA0    "All items in Northeast urban, urban wage earners and clerica…
#> 20 CWUR0000SEMD01 "Hospital services in U.S. city average, urban wage earners a…
```
