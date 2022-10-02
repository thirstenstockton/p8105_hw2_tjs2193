P8105 HW2
================
Thirsten Stockton
2022-10-02

# Problem 1

***Bring in our data***

LOADING PACKAGES AND SETTING PRINT OPTIONS FOR TIBBLES

``` r
library(tidyverse)

options(tibble.print_min = 5)
```

BRINGING IN OUR DATASET

``` r
nyc_transit_data = read_csv(file = "./NYC_Transit_Subway_Entrance_And_Exit_Data.csv")
```

    ## Rows: 1868 Columns: 32
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (22): Division, Line, Station Name, Route1, Route2, Route3, Route4, Rout...
    ## dbl  (8): Station Latitude, Station Longitude, Route8, Route9, Route10, Rout...
    ## lgl  (2): ADA, Free Crossover
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
nyc_transit_data
```

    ## # A tibble: 1,868 × 32
    ##   Division Line     Station…¹ Stati…² Stati…³ Route1 Route2 Route3 Route4 Route5
    ##   <chr>    <chr>    <chr>       <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 BMT      4 Avenue 25th St      40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>  
    ## 2 BMT      4 Avenue 25th St      40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>  
    ## 3 BMT      4 Avenue 36th St      40.7   -74.0 N      R      <NA>   <NA>   <NA>  
    ## 4 BMT      4 Avenue 36th St      40.7   -74.0 N      R      <NA>   <NA>   <NA>  
    ## 5 BMT      4 Avenue 36th St      40.7   -74.0 N      R      <NA>   <NA>   <NA>  
    ## # … with 1,863 more rows, 22 more variables: Route6 <chr>, Route7 <chr>,
    ## #   Route8 <dbl>, Route9 <dbl>, Route10 <dbl>, Route11 <dbl>,
    ## #   `Entrance Type` <chr>, Entry <chr>, `Exit Only` <chr>, Vending <chr>,
    ## #   Staffing <chr>, `Staff Hours` <chr>, ADA <lgl>, `ADA Notes` <chr>,
    ## #   `Free Crossover` <lgl>, `North South Street` <chr>,
    ## #   `East West Street` <chr>, Corner <chr>, `Entrance Latitude` <dbl>,
    ## #   `Entrance Longitude` <dbl>, `Station Location` <chr>, …
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
