433HW1
================

## How many flights have a missing dep\_time? What other variables are missing? What might these rows represent?

``` r
library(nycflights13)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.8
    ## v tidyr   1.2.0     v stringr 1.4.0
    ## v readr   2.1.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
filter(flights, is.na(dep_time))
```

    ## # A tibble: 8,255 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1       NA           1630        NA       NA           1815
    ##  2  2013     1     1       NA           1935        NA       NA           2240
    ##  3  2013     1     1       NA           1500        NA       NA           1825
    ##  4  2013     1     1       NA            600        NA       NA            901
    ##  5  2013     1     2       NA           1540        NA       NA           1747
    ##  6  2013     1     2       NA           1620        NA       NA           1746
    ##  7  2013     1     2       NA           1355        NA       NA           1459
    ##  8  2013     1     2       NA           1420        NA       NA           1644
    ##  9  2013     1     2       NA           1321        NA       NA           1536
    ## 10  2013     1     2       NA           1545        NA       NA           1910
    ## # ... with 8,245 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

The arr\_time and dep\_delay are also missing. If all these three data
are missing, there is a high probability that the flight was canceled.

## Currently dep\_time and sched\_dep\_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.

``` r
flights_time=mutate(flights,
                    dep_time_mins=(dep_time%/%100*60+dep_time%%100)%%1440,
                    sched_dep_time_mins=(sched_dep_time%/%100*60
                                         +sched_dep_time%%100)%%1440)
select(flights_time,dep_time,dep_time_mins,sched_dep_time,sched_dep_time_mins)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_time_mins sched_dep_time sched_dep_time_mins
    ##       <int>         <dbl>          <int>               <dbl>
    ##  1      517           317            515                 315
    ##  2      533           333            529                 329
    ##  3      542           342            540                 340
    ##  4      544           344            545                 345
    ##  5      554           354            600                 360
    ##  6      554           354            558                 358
    ##  7      555           355            600                 360
    ##  8      557           357            600                 360
    ##  9      557           357            600                 360
    ## 10      558           358            600                 360
    ## # ... with 336,766 more rows

## Look at the number of canceled flights per day. Is there a pattern? Is the proportion of canceled flights related to the average delay? Use multiple dyplr operations, all on one line, concluding with ggplot(aes(x=,y=))+geom\_point()

``` r
cancel_per_day=flights%>%
  mutate(cancel=(is.na(arr_delay)|is.na(dep_delay)))%>%
  group_by(year,month,day)%>%
  summarise(cancel_num=sum(cancel),flights_num=n())
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the
    ## `.groups` argument.

``` r
ggplot(cancel_per_day)+geom_point(aes(x=flights_num,y=cancel_num))
```

![](hw1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
cancel_and_delay=flights%>%
  mutate(cancel=(is.na(arr_delay)|is.na(dep_delay)))%>%
  group_by(year,month,day)%>%
  summarise(cancel_prob=mean(cancel),
            avg_dep_delay=mean(dep_delay,na.rm=T),
            avg_arr_delay=mean(arr_delay,na.rm=T))%>%ungroup()
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the
    ## `.groups` argument.

``` r
ggplot(cancel_and_delay)+geom_point(aes(x=avg_dep_delay,y=cancel_prob))
```

![](hw1_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
ggplot(cancel_and_delay)+geom_point(aes(x=avg_arr_delay,y=cancel_prob))
```

![](hw1_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->
