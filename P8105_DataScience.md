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

***Having issues locating “sports_balls”***

Combining datasets

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

Finding total weight collected by Professor Trash Wheel

``` r
mr_prof_trash_df %>%
  filter( join == "b" ) %>%
    select(weight_tons) %>%
      sum()
```

    ## [1] 190.12

Finding total number of sports balls collected by Mr. Trash Wheel in
2020

``` r
mr_prof_trash_df %>%
  filter( join == "a", year == 2020 ) %>%
    select(sports_balls) %>%
      sum()
```

    ## [1] 856

*This dataset has 641 observations and 17 variables. Key varaibales
include date of trash collection, and key stats about weight of trash
and types of trash collected. The amount of trash collected by Professor
Trash Wheel from 2017 to 2022 totals to 190.12 tons. The total amount of
sports balls collected by Mr. Trash Wheel in 2020 is 856.*

# Problem 3

``` r
politician_df = 
  read_csv(
    "./fivethirtyeight_datasets/pols-month.csv") %>%
  separate(mon, sep = "-", into = c("month", "day", "year")) %>%
    mutate( month = month.name[as.numeric(month)]) %>%
      mutate( president = recode(prez_gop, "0 " =  "dem", "1" =           "gop") ) %>%
            select(-prez_gop ) %>%
              select(-prez_dem)
```

    ## Rows: 823 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): mon
    ## dbl (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, re...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: Unreplaced values treated as NA as `.x` is not compatible.
    ## Please specify replacements exhaustively or supply `.default`.

``` r
ls(politician_df)
```

    ##  [1] "day"       "gov_dem"   "gov_gop"   "month"     "president" "rep_dem"  
    ##  [7] "rep_gop"   "sen_dem"   "sen_gop"   "year"

``` r
politician_df
```

    ## # A tibble: 823 × 10
    ##   month    day   year  gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##   <chr>    <chr> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ## 1 January  15    1947       23      51     253      23      45     198 dem      
    ## 2 February 15    1947       23      51     253      23      45     198 dem      
    ## 3 March    15    1947       23      51     253      23      45     198 dem      
    ## 4 April    15    1947       23      51     253      23      45     198 dem      
    ## 5 May      15    1947       23      51     253      23      45     198 dem      
    ## # … with 818 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

``` r
skimr::skim(politician_df)
```

|                                                  |               |
|:-------------------------------------------------|:--------------|
| Name                                             | politician_df |
| Number of rows                                   | 823           |
| Number of columns                                | 10            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |               |
| Column type frequency:                           |               |
| character                                        | 4             |
| numeric                                          | 6             |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |               |
| Group variables                                  | None          |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| month         |         1 |          1.00 |   3 |   9 |     0 |       12 |          0 |
| day           |         1 |          1.00 |   2 |   2 |     0 |        1 |          0 |
| year          |         1 |          1.00 |   4 |   4 |     0 |       69 |          0 |
| president     |         6 |          0.99 |   3 |   3 |     0 |        2 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |    sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-------:|------:|----:|----:|----:|----:|-----:|:------|
| gov_gop       |         1 |             1 |  22.48 |  5.68 |  12 |  18 |  22 |  28 |   34 | ▆▆▇▅▅ |
| sen_gop       |         1 |             1 |  46.10 |  6.38 |  32 |  42 |  46 |  51 |   56 | ▃▃▇▇▇ |
| rep_gop       |         1 |             1 | 194.92 | 29.24 | 141 | 176 | 195 | 222 |  253 | ▃▇▆▃▅ |
| gov_dem       |         1 |             1 |  27.20 |  5.94 |  17 |  22 |  28 |  32 |   41 | ▆▅▇▆▂ |
| sen_dem       |         1 |             1 |  54.41 |  7.37 |  44 |  48 |  53 |  58 |   71 | ▇▆▇▃▂ |
| rep_dem       |         1 |             1 | 244.97 | 31.37 | 188 | 211 | 250 | 268 |  301 | ▇▂▇▇▅ |

?month.name

?read.table
