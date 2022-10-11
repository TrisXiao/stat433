433HW2
================
Yang Xiao
2022-10-11

## What time of day should you fly if you want to avoid delays as much as possible? Does this choice depend on anything? Season? Weather? Airport? Airline? Find three patterns (“null results” are ok!). Write your results into Rmarkdown. Include a short introduction that summarizes the three results. Then, have a section for each finding. Support each finding with data summaries and visualizations. Include your code when necessary. This shouldn’t be long, but it might take some time to find the things you want to talk about and lay them out in an orderly way.

To avoid delays as much as possible, I think it depends on hour,
month(season), and airlines. Base on the results what I got below, the
best way to avoid delays is choose the AS or HA airlines in Autumn
around 5 to 7 am.

``` r
library("nycflights13")
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
flights %>% group_by(carrier) %>%
  summarise(arr_delay=mean(arr_delay,na.rm=T)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 16 × 2
    ##    carrier arr_delay
    ##    <chr>       <dbl>
    ##  1 AS         -9.93 
    ##  2 HA         -6.92 
    ##  3 AA          0.364
    ##  4 DL          1.64 
    ##  5 VX          1.76 
    ##  6 US          2.13 
    ##  7 UA          3.56 
    ##  8 9E          7.38 
    ##  9 B6          9.46 
    ## 10 WN          9.65 
    ## 11 MQ         10.8  
    ## 12 OO         11.9  
    ## 13 YV         15.6  
    ## 14 EV         15.8  
    ## 15 FL         20.1  
    ## 16 F9         21.9

``` r
flights %>% group_by(month) %>%
  summarise(arr_delay=mean(arr_delay,na.rm=T)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 12 × 2
    ##    month arr_delay
    ##    <int>     <dbl>
    ##  1     9    -4.02 
    ##  2    10    -0.167
    ##  3    11     0.461
    ##  4     5     3.52 
    ##  5     2     5.61 
    ##  6     3     5.81 
    ##  7     8     6.04 
    ##  8     1     6.13 
    ##  9     4    11.2  
    ## 10    12    14.9  
    ## 11     6    16.5  
    ## 12     7    16.7

``` r
flights %>% group_by(hour) %>%
  summarise(arr_delay=mean(arr_delay,na.rm=T)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 20 × 2
    ##     hour arr_delay
    ##    <dbl>     <dbl>
    ##  1     7    -5.30 
    ##  2     5    -4.80 
    ##  3     6    -3.38 
    ##  4     9    -1.45 
    ##  5     8    -1.11 
    ##  6    10     0.954
    ##  7    11     1.48 
    ##  8    12     3.49 
    ##  9    13     6.54 
    ## 10    14     9.20 
    ## 11    23    11.8  
    ## 12    15    12.3  
    ## 13    16    12.6  
    ## 14    18    14.8  
    ## 15    22    16.0  
    ## 16    17    16.0  
    ## 17    19    16.7  
    ## 18    20    16.7  
    ## 19    21    18.4  
    ## 20     1   NaN
