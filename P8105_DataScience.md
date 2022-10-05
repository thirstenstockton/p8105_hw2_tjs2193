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

skimr::skim(nyc_transit_data)
```

|                                                  |                  |
|:-------------------------------------------------|:-----------------|
| Name                                             | nyc_transit_data |
| Number of rows                                   | 1868             |
| Number of columns                                | 19               |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                  |
| Column type frequency:                           |                  |
| character                                        | 15               |
| logical                                          | 2                |
| numeric                                          | 2                |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                  |
| Group variables                                  | None             |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| line          |         0 |          1.00 |   5 |  17 |     0 |       36 |          0 |
| station_name  |         0 |          1.00 |   4 |  39 |     0 |      356 |          0 |
| route1        |         0 |          1.00 |   1 |   2 |     0 |       24 |          0 |
| route2        |       848 |          0.55 |   1 |   2 |     0 |       20 |          0 |
| route3        |      1374 |          0.26 |   1 |   2 |     0 |       18 |          0 |
| route4        |      1547 |          0.17 |   1 |   1 |     0 |       13 |          0 |
| route5        |      1630 |          0.13 |   1 |   1 |     0 |       12 |          0 |
| route6        |      1741 |          0.07 |   1 |   1 |     0 |        7 |          0 |
| route7        |      1788 |          0.04 |   1 |   2 |     0 |        7 |          0 |
| route8        |      1820 |          0.03 |   1 |   1 |     0 |        3 |          0 |
| route9        |      1840 |          0.01 |   1 |   1 |     0 |        2 |          0 |
| route10       |      1845 |          0.01 |   1 |   1 |     0 |        1 |          0 |
| route11       |      1845 |          0.01 |   1 |   1 |     0 |        1 |          0 |
| vending       |         0 |          1.00 |   2 |   3 |     0 |        2 |          0 |
| entrance_type |         0 |          1.00 |   4 |   9 |     0 |        7 |          0 |

**Variable type: logical**

| skim_variable | n_missing | complete_rate | mean | count               |
|:--------------|----------:|--------------:|-----:|:--------------------|
| entry         |         0 |             1 | 0.94 | TRU: 1753, FAL: 115 |
| ada           |         0 |             1 | 0.25 | FAL: 1400, TRU: 468 |

**Variable type: numeric**

| skim_variable     | n_missing | complete_rate |   mean |   sd |     p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:------------------|----------:|--------------:|-------:|-----:|-------:|-------:|-------:|-------:|-------:|:------|
| station_latitude  |         0 |             1 |  40.73 | 0.07 |  40.58 |  40.69 |  40.73 |  40.77 |  40.90 | ▂▅▇▃▂ |
| station_longitude |         0 |             1 | -73.94 | 0.06 | -74.03 | -73.99 | -73.96 | -73.91 | -73.76 | ▇▆▃▂▁ |
