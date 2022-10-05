P8105 HW2
================
Thirsten Stockton
2022-10-02

# Problem 1

***Importing and Cleaning Data***

LOADING PACKAGES AND SETTING PRINT OPTIONS FOR TIBBLES

``` r
library(tidyverse)
library(readxl)
options(tibble.print_min = 5)
```

BRINGING IN OUR DATASET

``` r
nyc_transit_data = 
  read_csv(
    "./NYC_Transit_Subway_Entrance_And_Exit_Data.csv",
  col_types = cols(Route8 = "c", Route9 = "c", Route10 = "c",     Route11 = "c"))%>%
  janitor::clean_names() %>%
  select(
    line,
    station_name,
    station_latitude,
    station_longitude,
    contains("Route"),
    entry,
    vending,
    entrance_type,
    ada)%>%
   mutate(
     entry = ifelse(entry == "YES", TRUE, FALSE))

nyc_transit_data
```

    ## # A tibble: 1,868 × 19
    ##   line  stati…¹ stati…² stati…³ route1 route2 route3 route4 route5 route6 route7
    ##   <chr> <chr>     <dbl>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
    ## 1 4 Av… 25th St    40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 2 4 Av… 25th St    40.7   -74.0 R      <NA>   <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 3 4 Av… 36th St    40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 4 4 Av… 36th St    40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## 5 4 Av… 36th St    40.7   -74.0 N      R      <NA>   <NA>   <NA>   <NA>   <NA>  
    ## # … with 1,863 more rows, 8 more variables: route8 <chr>, route9 <chr>,
    ## #   route10 <chr>, route11 <chr>, entry <lgl>, vending <chr>,
    ## #   entrance_type <chr>, ada <lgl>, and abbreviated variable names
    ## #   ¹​station_name, ²​station_latitude, ³​station_longitude
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

This dataset contains variables pertaining to subway line, station name,
station latitude, station longitude, Route#, entry, vending, entrance
type and ada compliance. So far, the csv file has been read in, column
types where changed for “Route” variables that were coded as numeric,
the data set was restricted to the above variables, and entry was
changed to a logical variable. The dataframe dimensions are 19x1868. The
data is not tidy because the “Route” variables need to be changed from
wide format to long format.
