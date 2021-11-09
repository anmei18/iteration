list columns
================
AnMei Chen
11/5/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

options(
  ggplot2.continuous.colour = "viridis" , 
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Define functions

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
    
  return(output_df)
  
}
```

## Lists

``` r
l = 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE,FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.408   2.982   4.985   5.037   7.056  14.766

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.408   2.982   4.985   5.037   7.056  14.766

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.408   2.982   4.985   5.037   7.056  14.766

## list of normals

``` r
list_norms =
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.81 0.894

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.10  2.74

``` r
mean_and_sd(list_norms[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.81 0.894

## for loop

Let’s use a for loop to iterate over my list of normals

``` r
#create a blank list
output = vector("list", length = 4)

#add something to the first spot of the list "output"
output[[1]] = mean_and_sd(list_norms[[1]])


for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norms[[i]])
  
}
```

Let’s use map instead …

``` r
# This is the same as the for loop
output = map(list_norms, mean_and_sd)

output = map(list_norms, median)

output = map(list_norms, summary)

output = map_dbl(list_norms, median)
```

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a","b","c","d"),
    norms = list_norms
  )

listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  2.3905725  1.0613537  2.9079893  2.1290200 -1.1178594  1.1574384
    ##  [7]  2.8780200  1.9284584  2.5786565  1.3081852  2.9816557  2.5838842
    ## [13]  1.4830689  1.2255896  2.4695776  1.5826489  1.1592467  1.3002148
    ## [19]  1.4507196  1.6368738  2.7072074  2.8983468  2.5550088  0.9088938
    ## [25]  0.6391284  0.6231194  2.2985456  0.7473217  2.2075003  3.1161243
    ## [31]  1.2802894  2.9878259  2.0636072  1.6122827  1.5431078  0.4396940
    ## [37]  2.5562309  0.6535199  2.9516680  1.5905301  1.2282319  0.9998607
    ## [43]  1.5477572  2.2150771  1.4128804  2.3544142  3.1895751  2.1870858
    ## [49]  2.9901538  0.9239287
    ## 
    ## $b
    ##  [1]  8.2614442  6.4633725 11.7933753  7.1805025  4.5056692  4.0614723
    ##  [7]  9.7272245  5.3469917  4.3101217  2.3276259  2.8303883  3.0211057
    ## [13]  7.6024923  3.2467847  2.8003061  1.9574469  3.7138744 10.1963151
    ## [19]  9.1668825  4.0148915  5.9851920  9.4207554  4.7728061  3.5597884
    ## [25]  2.9086339  4.3440256  5.2956101  1.2928425  0.3145104  6.5484032
    ## [31]  5.4934690  7.4484833  6.0637779  6.7278002  8.1722641  4.5376687
    ## [37]  6.6693184  0.1453305  8.5572776  1.9679889  4.5497952  3.7906882
    ## [43]  1.6055899  3.4172796  1.9475521  2.9136217  4.5211456  2.6309452
    ## [49]  8.4784591  8.5013908
    ## 
    ## $c
    ##  [1] 19.40327 20.53707 21.33873 22.01064 18.72964 18.56655 19.50115 20.00546
    ##  [9] 20.34929 19.96061 18.77262 20.34510 20.31028 20.43799 19.36700 21.21872
    ## [17] 21.29373 18.22139 20.10177 17.34413 19.18777 19.42517 17.39876 20.43799
    ## [25] 22.03679 20.88624 20.08224 21.19400 20.21302 20.92399 19.66839 20.73981
    ## [33] 20.96804 20.76201 21.21766 20.78431 20.15004 19.77510 19.09575 21.01305
    ## [41] 20.36130 19.58511 20.34091 19.13994 18.85429 21.52104 20.51651 20.85951
    ## [49] 18.42450 19.30466
    ## 
    ## $d
    ##  [1] -11.87515 -12.27349 -12.09169 -11.89402 -11.68621 -12.15423 -11.03693
    ##  [8] -11.85819 -11.48601 -12.75434 -12.43417 -12.16066 -12.63644 -12.17788
    ## [15] -12.71865 -12.61978 -11.69010 -12.25079 -12.74617 -11.88642 -12.17245
    ## [22] -10.66794 -12.60871 -12.35413 -12.76932 -12.56517 -12.12846 -11.94667
    ## [29] -11.73964 -11.07289 -11.67307 -11.96427 -12.91831 -11.33546 -11.09110
    ## [36] -11.88183 -12.32111 -11.21721 -11.85201 -12.36133 -10.64748 -12.65733
    ## [43] -12.07483 -12.11355 -11.64230 -11.65148 -11.83143 -12.30763 -11.52995
    ## [50] -11.59960

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.81 0.894

``` r
listcol_df %>% 
  mutate(summaries = map(listcol_df$norms, mean_and_sd))
```

    ## # A tibble: 4 × 3
    ##   name  norms        summaries       
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [50]>   <tibble [1 × 2]>
    ## 2 b     <dbl [50]>   <tibble [1 × 2]>
    ## 3 c     <dbl [50]>   <tibble [1 × 2]>
    ## 4 d     <dbl [50]>   <tibble [1 × 2]>

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:31:26 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:31:33 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:31:36 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nested = nest(weather_df, data = date:tmin)

weather_nested =
  weather_df %>% 
  nest(data = date:tmin)

unnest(weather_nested,data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
weather_nested %>% 
  filter(name == "CentralPark_NY") %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

## Napolean

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)
  
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url,1:5)

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  2 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  3 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  4 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  5 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  6 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ##  7 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi…
    ##  8 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie…
    ##  9 Watch it twice! Trust me!         5.0 out of 5 stars "\n  Nothing to dislike…
    ## 10 A classic                         5.0 out of 5 stars "\n  If you don’t enjoy…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  2 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  3 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  4 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  5 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ##  6 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  7 Love it                                     5.0 out of 5 stars "\n  What of …
    ##  8 WORTH IT!                                   5.0 out of 5 stars "\n  It's the…
    ##  9 Funny movie.                                5.0 out of 5 stars "\n  Great co…
    ## 10 Best movie ever!                            5.0 out of 5 stars "\n  Got this…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  2 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  3 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  4 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  5 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ##  6 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ##  7 Love this movie!                              5.0 out of 5 stars "\n  Great …
    ##  8 Love it                                       5.0 out of 5 stars "\n  Love t…
    ##  9 As described                                  3.0 out of 5 stars "\n  Book i…
    ## 10 GOSH!!!                                       5.0 out of 5 stars "\n  Just w…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  2 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  3 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  4 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  5 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  6 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  7 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ##  8 So Funny                          5.0 out of 5 stars "\n  This is such a goo…
    ##  9 Best movie ever                   5.0 out of 5 stars "\n  It's napoleon dyna…
    ## 10 Funny                             5.0 out of 5 stars "\n  Classic\n"

``` r
napoleon_df = 
  tibble(
    urls = urls
  )

napoleon_df %>% 
  mutate( reviews = map(urls, get_page_reviews)) %>% 
  select(reviews) %>% 
  unnest(cols = reviews)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## # … with 40 more rows
