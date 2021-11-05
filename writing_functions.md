writing\_functions
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

## Z scores

``` r
x_vec = rnorm(25,mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.23903117 -0.34844162  0.61240065  1.53808226 -1.76558141 -0.73931921
    ##  [7]  1.29232495 -0.91450744  1.00170100 -3.04128200 -0.76503806  0.46836735
    ## [13] -0.45069278  0.84726338  0.40144010 -0.05338857 -0.82510902 -0.55469626
    ## [19]  0.70820656 -0.10453249  0.63382031  0.31554517  0.54800456  0.41474773
    ## [25]  0.54165366

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.23903117 -0.34844162  0.61240065  1.53808226 -1.76558141 -0.73931921
    ##  [7]  1.29232495 -0.91450744  1.00170100 -3.04128200 -0.76503806  0.46836735
    ## [13] -0.45069278  0.84726338  0.40144010 -0.05338857 -0.82510902 -0.55469626
    ## [19]  0.70820656 -0.10453249  0.63382031  0.31554517  0.54800456  0.41474773
    ## [25]  0.54165366

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_scores(x = y_vec)
```

    ##  [1]  1.4922651  1.2596036 -1.4776877 -1.8084390  0.4874585 -1.3530138
    ##  [7] -0.4154192  0.3860172 -1.4834795  0.2498590  0.4468829  1.1620955
    ## [13] -1.1256077  0.3840310  0.3270431 -1.0652073  1.2730578 -0.6292500
    ## [19]  0.2175511 -0.8327925  0.8315643  1.6596906  0.7335456  0.9412061
    ## [25]  0.1303114  0.7070990  0.7565176  0.1320549 -0.5292226 -0.2636893
    ## [31] -2.3447733 -1.3994085  0.6051060  1.4125018 -0.4719865  0.4090063
    ## [37]  0.7004495 -0.9459078 -0.4211158 -0.1379174

How great is this? Only kinda great. Let’s try again.

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores(c("my","name","is", "jeff"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
mtcars
```

    ##                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    ## Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    ## Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    ## Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    ## Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    ## Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    ## Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    ## Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    ## Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    ## Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    ## Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    ## Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

## Mutiple outputs

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

mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.274

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.21  4.05

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  2.90

Let’s write a function that simulates data, computes the mean and sd.

(shift + option helps you type at mutiple lines at the same time)

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4){
  
  #do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

# if you didnt specified, r assume you put things in order: n = 30, mu = 40, sigma = 3 (positional matching)
sim_mean_sd(30,40,3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.0  2.86

## Napolean Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
```

Okay but there are a lot of pages of reviews..

Write a function that gets reviews based on page url

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

# page 1 (change the number at the end):
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  2 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  3 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  4 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  5 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  6 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  7 Painful                                               1.0 ou… "\n  I think I…
    ##  8 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  9 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ## 10 Cult Classic                                          5.0 ou… "\n  Watched i…

``` r
# page 2:
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Format was inaccurate                       4.0 out of 5 stars "\n  There wa…
    ##  2 Good funny                                  3.0 out of 5 stars "\n  Would re…
    ##  3 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn…
    ##  4 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  5 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  6 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  7 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  8 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  9 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ## 10 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…

``` r
# OR:

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url,1:5)

get_page_reviews(urls[1])
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  2 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  3 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  4 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  5 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  6 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  7 Painful                                               1.0 ou… "\n  I think I…
    ##  8 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  9 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ## 10 Cult Classic                                          5.0 ou… "\n  Watched i…

``` r
get_page_reviews(urls[2])
```

    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Format was inaccurate                       4.0 out of 5 stars "\n  There wa…
    ##  2 Good funny                                  3.0 out of 5 stars "\n  Would re…
    ##  3 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn…
    ##  4 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  5 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  6 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  7 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  8 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  9 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ## 10 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…

``` r
get_page_reviews(urls[3])
```

    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Okay                                          3.0 out of 5 stars "\n  Okay\n"
    ##  2 A WHOLESOME comedic journey                   5.0 out of 5 stars "\n  Not a …
    ##  3 Hilarious                                     5.0 out of 5 stars "\n  Funny\…
    ##  4 Love it                                       5.0 out of 5 stars "\n  What o…
    ##  5 WORTH IT!                                     5.0 out of 5 stars "\n  It's t…
    ##  6 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  7 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  8 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  9 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ## 10 Still funny!                                  5.0 out of 5 stars "\n  Still …

``` r
get_page_reviews(urls[4])
```

    ## # A tibble: 10 × 3
    ##    title                           stars              text                      
    ##    <chr>                           <chr>              <chr>                     
    ##  1 Love it!! 💜                    5.0 out of 5 stars "\n  Love it!! 💜\n"      
    ##  2 LOVE it                         5.0 out of 5 stars "\n  cult classic. So ugl…
    ##  3 Perfect                         5.0 out of 5 stars "\n  Exactly what I asked…
    ##  4 Love this movie!                5.0 out of 5 stars "\n  Great movie and sent…
    ##  5 Love it                         5.0 out of 5 stars "\n  Love this movie. How…
    ##  6 As described                    3.0 out of 5 stars "\n  Book is as described…
    ##  7 GOSH!!!                         5.0 out of 5 stars "\n  Just watch the movie…
    ##  8 Watch it right now              5.0 out of 5 stars "\n  You need to watch th…
    ##  9 At this point it’s an addiction 5.0 out of 5 stars "\n  I watch this movie w…
    ## 10 💕                              5.0 out of 5 stars "\n  Hands down, one of m…

``` r
get_page_reviews(urls[5])
```

    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  2 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  3 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  4 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ##  5 So Funny                          5.0 out of 5 stars "\n  This is such a goo…
    ##  6 Best movie ever                   5.0 out of 5 stars "\n  It's napoleon dyna…
    ##  7 Funny                             5.0 out of 5 stars "\n  Classic\n"         
    ##  8 It’s broke!                       1.0 out of 5 stars "\n  I don’t know if yo…
    ##  9 Stupid                            1.0 out of 5 stars "\n  What can I say? St…
    ## 10 Not funny                         1.0 out of 5 stars "\n  Not funny\n"

``` r
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  2 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  3 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  4 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  5 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  6 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  7 Painful                                               1.0 ou… "\n  I think I…
    ##  8 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  9 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ## 10 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## # … with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
