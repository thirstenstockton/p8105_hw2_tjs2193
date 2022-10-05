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
```

This dataset contains variables pertaining to subway line, station name,
station latitude, station longitude, Route#, entry, vending, entrance
type and ada compliance. So far, the csv file has been read in, column
types where changed for “Route” variables that were coded as numeric,
the data set was restricted to the above variables, and entry was
changed to a logical variable. The dataframe dimensions are 19x1868. The
data is not tidy because the “Route” variables need to be changed from
wide format to long format.

***Questions***

How many distinct stations are there? Note that stations are identified
both by name and by line (e.g. 125th St 8th Avenue; 125st Broadway;
125st Lenox); the distinct function may be useful here. ***465***

``` r
nyc_transit_data %>% 
  select(station_name, line) %>% 
    distinct
```

    ## # A tibble: 465 × 2
    ##   station_name line    
    ##   <chr>        <chr>   
    ## 1 25th St      4 Avenue
    ## 2 36th St      4 Avenue
    ## 3 45th St      4 Avenue
    ## 4 53rd St      4 Avenue
    ## 5 59th St      4 Avenue
    ## # … with 460 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

How many stations are ADA compliant? ***84***

``` r
nyc_transit_data %>% 
  filter(ada == TRUE) %>% 
    select(station_name, line) %>% 
      distinct
```

    ## # A tibble: 84 × 2
    ##   station_name             line           
    ##   <chr>                    <chr>          
    ## 1 Atlantic Av-Barclays Ctr 4 Avenue       
    ## 2 DeKalb Av                4 Avenue       
    ## 3 Pacific St               4 Avenue       
    ## 4 Grand Central            42nd St Shuttle
    ## 5 34th St                  6 Avenue       
    ## # … with 79 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

What proportion of station entrances / exits without vending allow
entrance? ***37.7%***

``` r
nyc_transit_data %>% 
  filter(vending == "NO") %>% 
    pull(entry) %>% 
      mean
```

    ## [1] 0.3770492
