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

Reformatted Data: Distinct stations that serve the A train ***60***

``` r
nyc_transit_data %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A") %>% 
  select(station_name, line, route_num) %>% 
  distinct
```

    ## # A tibble: 60 × 3
    ##   station_name                  line            route_num
    ##   <chr>                         <chr>           <chr>    
    ## 1 Times Square                  42nd St Shuttle route1   
    ## 2 125th St                      8 Avenue        route1   
    ## 3 145th St                      8 Avenue        route1   
    ## 4 14th St                       8 Avenue        route1   
    ## 5 168th St - Washington Heights 8 Avenue        route1   
    ## # … with 55 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

Reformatted Data: Distinct stations that serve the A train that are ada
compliant ***17***

``` r
nyc_transit_data %>% 
  pivot_longer(
    route1:route11,
    names_to = "route_num",
    values_to = "route") %>% 
  filter(route == "A", ada == TRUE) %>% 
  select(station_name, line) %>% 
  distinct
```

    ## # A tibble: 17 × 2
    ##    station_name                  line            
    ##    <chr>                         <chr>           
    ##  1 14th St                       8 Avenue        
    ##  2 168th St - Washington Heights 8 Avenue        
    ##  3 175th St                      8 Avenue        
    ##  4 34th St                       8 Avenue        
    ##  5 42nd St                       8 Avenue        
    ##  6 59th St                       8 Avenue        
    ##  7 Inwood - 207th St             8 Avenue        
    ##  8 West 4th St                   8 Avenue        
    ##  9 World Trade Center            8 Avenue        
    ## 10 Times Square-42nd St          Broadway        
    ## 11 59th St-Columbus Circle       Broadway-7th Ave
    ## 12 Times Square                  Broadway-7th Ave
    ## 13 8th Av                        Canarsie        
    ## 14 Franklin Av                   Franklin        
    ## 15 Euclid Av                     Fulton          
    ## 16 Franklin Av                   Fulton          
    ## 17 Howard Beach                  Rockaway

# Problem 2

??read_xl

``` r
trash_wheel_df = read_excel("./Trash Wheel Collection Data.xlsx",
    sheet = "Mr. Trash Wheel")
```

    ## New names:
    ## • `` -> `...15`
    ## • `` -> `...16`

``` r
skimr::skim(trash_wheel_df)
```

|                                                  |                |
|:-------------------------------------------------|:---------------|
| Name                                             | trash_wheel_df |
| Number of rows                                   | 548            |
| Number of columns                                | 16             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                |
| Column type frequency:                           |                |
| character                                        | 2              |
| logical                                          | 2              |
| numeric                                          | 11             |
| POSIXct                                          | 1              |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                |
| Group variables                                  | None           |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| Month         |         1 |             1 |   3 |   9 |     0 |       13 |          0 |
| Year          |         1 |             1 |   4 |   4 |     0 |        9 |          0 |

**Variable type: logical**

| skim_variable | n_missing | complete_rate | mean | count |
|:--------------|----------:|--------------:|-----:|:------|
| …15           |       548 |             0 |  NaN | :     |
| …16           |       548 |             0 |  NaN | :     |

**Variable type: numeric**

| skim_variable        | n_missing | complete_rate |     mean |        sd |     p0 |     p25 |     p50 |      p75 |        p100 | hist  |
|:---------------------|----------:|--------------:|---------:|----------:|-------:|--------:|--------:|---------:|------------:|:------|
| Dumpster             |         1 |          1.00 |   274.00 |    158.05 |   1.00 |  137.50 |  274.00 |   410.50 |      547.00 | ▇▇▇▇▇ |
| Weight (tons)        |         0 |          1.00 |     6.38 |     74.55 |   0.78 |    2.71 |    3.19 |     3.73 |     1748.36 | ▇▁▁▁▁ |
| Volume (cubic yards) |         0 |          1.00 |    30.60 |    357.54 |   7.00 |   15.00 |   15.00 |    15.00 |     8385.00 | ▇▁▁▁▁ |
| Plastic Bottles      |         0 |          1.00 |  3909.84 |  45692.15 | 210.00 |  980.00 | 1880.00 |  2745.00 |  1071295.00 | ▇▁▁▁▁ |
| Polystyrene          |         0 |          1.00 |  3292.14 |  38482.28 |  48.00 |  702.50 | 1265.00 |  2500.00 |   902045.00 | ▇▁▁▁▁ |
| Cigarette Butts      |         0 |          1.00 | 41773.80 | 488988.61 | 900.00 | 4000.00 | 7000.00 | 27000.00 | 11446020.00 | ▇▁▁▁▁ |
| Glass Bottles        |         0 |          1.00 |    43.23 |    505.29 |   0.00 |   10.00 |   18.00 |    31.00 |    11844.00 | ▇▁▁▁▁ |
| Grocery Bags         |         0 |          1.00 |  1927.54 |  22535.84 |  24.00 |  330.00 |  684.00 |  1377.50 |   528146.00 | ▇▁▁▁▁ |
| Chip Bags            |         0 |          1.00 |  2832.00 |  33099.97 | 180.00 |  740.00 | 1100.00 |  1982.00 |   775969.00 | ▇▁▁▁▁ |
| Sports Balls         |         0 |          1.00 |    25.10 |    293.45 |   0.00 |    6.00 |   11.00 |    18.00 |     6878.60 | ▇▁▁▁▁ |
| Homes Powered\*      |        61 |          0.89 |    91.76 |   1010.65 |   0.00 |   39.58 |   51.17 |    59.33 |    22344.00 | ▇▁▁▁▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| Date          |         1 |             1 | 1900-01-20 | 2022-07-29 | 2018-07-18 |      331 |
