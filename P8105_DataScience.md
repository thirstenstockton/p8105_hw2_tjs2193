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

Mr. Trash Wheel

``` r
trash_wheel_df = read_excel("./Trash Wheel Collection Data.xlsx",
    sheet = "Mr. Trash Wheel") %>%
      janitor::clean_names() %>%
        drop_na (dumpster) %>%
           mutate (join = "a") 
        

trash_wheel_df
```

    ## # A tibble: 547 × 17
    ##   dumpster month  year date                weight_tons volume_…¹ plast…² polys…³
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>     <dbl>   <dbl>   <dbl>
    ## 1        1 May    2014 2014-05-16 00:00:00        4.31        18    1450    1820
    ## 2        2 May    2014 2014-05-16 00:00:00        2.74        13    1120    1030
    ## 3        3 May    2014 2014-05-16 00:00:00        3.45        15    2450    3100
    ## 4        4 May    2014 2014-05-17 00:00:00        3.1         15    2380    2730
    ## 5        5 May    2014 2014-05-17 00:00:00        4.06        18     980     870
    ## # … with 542 more rows, 9 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>, x16 <lgl>, join <chr>,
    ## #   and abbreviated variable names ¹​volume_cubic_yards, ²​plastic_bottles,
    ## #   ³​polystyrene
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

Professor Trash Wheel

``` r
p_trash_wheel_df = read_excel("./Trash Wheel Collection Data.xlsx",
    sheet = "Professor Trash Wheel") %>%
      janitor::clean_names() %>%
        drop_na (dumpster) %>%
          mutate (join = "b") 
        
        
 p_trash_wheel_df     
```

    ## # A tibble: 94 × 14
    ##   dumpster month     year date                weight_t…¹ volum…² plast…³ polys…⁴
    ##      <dbl> <chr>    <dbl> <dttm>                   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1        1 January   2017 2017-01-02 00:00:00       1.79      15    1950    6080
    ## 2        2 January   2017 2017-01-30 00:00:00       1.58      15    9540   11230
    ## 3        3 February  2017 2017-02-26 00:00:00       2.32      18    8350    9210
    ## 4        4 February  2017 2017-02-26 00:00:00       3.72      15    8590    1030
    ## 5        5 February  2017 2017-02-28 00:00:00       1.45      15    7830    9950
    ## # … with 89 more rows, 6 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   homes_powered <dbl>, join <chr>, and abbreviated variable names
    ## #   ¹​weight_tons, ²​volume_cubic_yards, ³​plastic_bottles, ⁴​polystyrene
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

\_

***Having issues locating “sports_balls”***

``` r
mr_prof_trash_df =
  bind_rows(trash_wheel_df, p_trash_wheel_df)

mr_prof_trash_df
```

    ## # A tibble: 641 × 17
    ##   dumpster month  year date                weight_tons volume_…¹ plast…² polys…³
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>     <dbl>   <dbl>   <dbl>
    ## 1        1 May    2014 2014-05-16 00:00:00        4.31        18    1450    1820
    ## 2        2 May    2014 2014-05-16 00:00:00        2.74        13    1120    1030
    ## 3        3 May    2014 2014-05-16 00:00:00        3.45        15    2450    3100
    ## 4        4 May    2014 2014-05-17 00:00:00        3.1         15    2380    2730
    ## 5        5 May    2014 2014-05-17 00:00:00        4.06        18     980     870
    ## # … with 636 more rows, 9 more variables: cigarette_butts <dbl>,
    ## #   glass_bottles <dbl>, grocery_bags <dbl>, chip_bags <dbl>,
    ## #   sports_balls <dbl>, homes_powered <dbl>, x15 <lgl>, x16 <lgl>, join <chr>,
    ## #   and abbreviated variable names ¹​volume_cubic_yards, ²​plastic_bottles,
    ## #   ³​polystyrene
    ## # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names

``` r
skimr::skim(mr_prof_trash_df)
```

|                                                  |                  |
|:-------------------------------------------------|:-----------------|
| Name                                             | mr_prof_trash_df |
| Number of rows                                   | 641              |
| Number of columns                                | 17               |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                  |
| Column type frequency:                           |                  |
| character                                        | 2                |
| logical                                          | 2                |
| numeric                                          | 12               |
| POSIXct                                          | 1                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                  |
| Group variables                                  | None             |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| month         |         0 |             1 |   3 |   9 |     0 |       13 |          0 |
| join          |         0 |             1 |   1 |   1 |     0 |        2 |          0 |

**Variable type: logical**

| skim_variable | n_missing | complete_rate | mean | count |
|:--------------|----------:|--------------:|-----:|:------|
| x15           |       641 |             0 |  NaN | :     |
| x16           |       641 |             0 |  NaN | :     |

**Variable type: numeric**

| skim_variable      | n_missing | complete_rate |     mean |       sd |      p0 |     p25 |     p50 |      p75 |      p100 | hist  |
|:-------------------|----------:|--------------:|---------:|---------:|--------:|--------:|--------:|---------:|----------:|:------|
| dumpster           |         0 |          1.00 |   240.78 |   166.88 |    1.00 |   81.00 |  227.00 |   387.00 |    547.00 | ▇▅▅▅▅ |
| year               |         0 |          1.00 |  2018.14 |     2.31 | 2014.00 | 2016.00 | 2018.00 |  2020.00 |   2022.00 | ▆▆▆▇▆ |
| weight_tons        |         0 |          1.00 |     3.02 |     0.84 |    0.61 |    2.48 |    3.08 |     3.62 |      5.62 | ▁▅▇▅▁ |
| volume_cubic_yards |         0 |          1.00 |    15.22 |     1.44 |    6.00 |   15.00 |   15.00 |    15.00 |     20.00 | ▁▁▁▇▁ |
| plastic_bottles    |         0 |          1.00 |  2464.81 |  1817.94 |  210.00 | 1110.00 | 2110.00 |  3100.00 |   9830.00 | ▇▆▁▁▁ |
| polystyrene        |         0 |          1.00 |  2088.81 |  1990.25 |   48.00 |  780.00 | 1460.00 |  2870.00 |  11528.00 | ▇▃▁▁▁ |
| cigarette_butts    |         0 |          1.00 | 19663.80 | 28187.00 |  900.00 | 4400.00 | 8000.00 | 23000.00 | 310000.00 | ▇▁▁▁▁ |
| glass_bottles      |         0 |          1.00 |    20.71 |    15.82 |    0.00 |    9.00 |   18.00 |    28.00 |    110.00 | ▇▃▁▁▁ |
| grocery_bags       |         0 |          1.00 |  1217.66 |  1634.36 |   24.00 |  360.00 |  780.00 |  1480.00 |  13450.00 | ▇▁▁▁▁ |
| chip_bags          |         0 |          1.00 |  2405.54 |  3050.01 |  180.00 |  800.00 | 1340.00 |  2684.00 |  20100.00 | ▇▁▁▁▁ |
| sports_balls       |        94 |          0.85 |    12.58 |     9.27 |    0.00 |    6.00 |   11.00 |    18.00 |     56.00 | ▇▅▂▁▁ |
| homes_powered      |        73 |          0.89 |    44.11 |    20.73 |    0.00 |   34.67 |   49.00 |    57.50 |     93.67 | ▂▃▇▅▁ |

**Variable type: POSIXct**

| skim_variable | n_missing | complete_rate | min        | max        | median     | n_unique |
|:--------------|----------:|--------------:|:-----------|:-----------|:-----------|---------:|
| date          |         0 |             1 | 1900-01-20 | 2022-07-29 | 2018-08-09 |      359 |

??read_excel ?as.character
