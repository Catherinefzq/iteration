Iteration and listcols
================
Catherine
10/30/2018

#### for loop

``` r
df = data_frame( # creat a data frame
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

is.list(df)
```

    ## [1] TRUE

``` r
df[[2]]
```

    ##  [1]  4.5948869  3.9106815  0.3728249 -9.9467585  3.0991287 -0.2806437
    ##  [7] -0.7789775 -7.3537619 -2.3907503  2.0897078  6.7933978 -0.5139386
    ## [13]  1.9383581 -0.2690252 -6.8852978 -2.0749728 -1.9714498 -0.2965670
    ## [19]  5.5001269  3.8158787

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  tibble(
  mean_x = mean(x),
  sd_x = sd(x)
  )

}
```

apply the function

``` r
mean_and_sd(df[[1]])
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   3.19 0.913

``` r
mean_and_sd(df[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean_x  sd_x
    ##     <dbl> <dbl>
    ## 1 -0.0324  4.36

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   10.0 0.162

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1  -2.90  1.05

``` r
output = vector("list", length = 4)

output[[1]] = mean_and_sd(df[[1]])
output[[2]] = mean_and_sd(df[[2]])
output[[3]] = mean_and_sd(df[[3]])
output[[4]] = mean_and_sd(df[[4]])

for (i in 1:4) {
  output[[i]] = mean_and_sd(df[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   3.19 0.913
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean_x  sd_x
    ##     <dbl> <dbl>
    ## 1 -0.0324  4.36
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   10.0 0.162
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1  -2.90  1.05

replace the 'for' loop with 'map'

``` r
output_map = map(df, mean_and_sd)
output_map
```

    ## $a
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   3.19 0.913
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean_x  sd_x
    ##     <dbl> <dbl>
    ## 1 -0.0324  4.36
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1   10.0 0.162
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1  -2.90  1.05

Try a different function

``` r
output_med = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = median(df[[i]])
}

output_med = map_dbl(df, median) # map_df 
output_med
```

    ##          a          b          c          d 
    ##  3.3596755 -0.2748345 10.0228773 -2.9488283

#### code syntax

be clear about arguements

``` r
output = map(.x = df, ~ mean_and_sd(x = .x)) # it is different from the x in function mean_and_sd
```

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

# for loop

output = vector("list", 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}
dynamite_reviews = bind_rows(output)

# map
output_review = map_df(.x = vec_urls, read_page_reviews)
```

``` r
library(rnoaa)

weather = 
  meteo_pull_monitors(c("USW00094728", "USC00519397", "USS0023B17S"),
                      var = c("PRCP", "TMIN", "TMAX"), 
                      date_min = "2016-01-01",
                      date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

``` r
weather_nest = weather %>% 
  group_by(name, id) %>% 
  nest(date:tmin)

weather_nest %>% 
  pull(id)
```

    ## [1] "USW00094728" "USC00519397" "USS0023B17S"

``` r
weather_nest %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # ... with 356 more rows
    ## 
    ## [[2]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0  29.4  16.7
    ##  2 2016-01-02     0  28.3  16.7
    ##  3 2016-01-03     0  28.3  16.7
    ##  4 2016-01-04     0  28.3  16.1
    ##  5 2016-01-05     0  27.2  16.7
    ##  6 2016-01-06     0  27.2  20  
    ##  7 2016-01-07    46  27.8  18.3
    ##  8 2016-01-08     3  28.3  17.8
    ##  9 2016-01-09     8  27.8  19.4
    ## 10 2016-01-10     3  28.3  18.3
    ## # ... with 356 more rows
    ## 
    ## [[3]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   1.7  -5.9
    ##  2 2016-01-02    25  -0.1  -6  
    ##  3 2016-01-03     0  -5   -10  
    ##  4 2016-01-04    25   0.3  -9.8
    ##  5 2016-01-05    25   1.9  -1.8
    ##  6 2016-01-06    25   1.4  -2.6
    ##  7 2016-01-07     0   1.4  -3.9
    ##  8 2016-01-08     0   1.1  -4  
    ##  9 2016-01-09     0   1.4  -4.5
    ## 10 2016-01-10     0   2.3  -3.8
    ## # ... with 356 more rows

unnesting...

``` r
weather_nest %>% 
  unnest
```

    ## # A tibble: 1,098 x 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2016-01-01     0   5.6   1.1
    ##  2 CentralPark_NY USW00094728 2016-01-02     0   4.4   0  
    ##  3 CentralPark_NY USW00094728 2016-01-03     0   7.2   1.7
    ##  4 CentralPark_NY USW00094728 2016-01-04     0   2.2  -9.9
    ##  5 CentralPark_NY USW00094728 2016-01-05     0  -1.6 -11.6
    ##  6 CentralPark_NY USW00094728 2016-01-06     0   5    -3.8
    ##  7 CentralPark_NY USW00094728 2016-01-07     0   7.8  -0.5
    ##  8 CentralPark_NY USW00094728 2016-01-08     0   7.8  -0.5
    ##  9 CentralPark_NY USW00094728 2016-01-09     0   8.3   4.4
    ## 10 CentralPark_NY USW00094728 2016-01-10   457  15     4.4
    ## # ... with 1,088 more rows

``` r
weather_nest$data[[1]] %>% # it's a data frame 
  skimr::skim()
```

    ## Skim summary statistics
    ##  n obs: 366 
    ##  n variables: 4 
    ## 
    ## ── Variable type:Date ───────────────────────────────────────────────────────────────────────────
    ##  variable missing complete   n        min        max     median n_unique
    ##      date       0      366 366 2016-01-01 2016-12-31 2016-07-01      366
    ## 
    ## ── Variable type:numeric ────────────────────────────────────────────────────────────────────────
    ##  variable missing complete   n  mean    sd    p0   p25   p50  p75  p100
    ##      prcp       0      366 366 29.3  78.66   0    0     0    10   587  
    ##      tmax       0      366 366 18.13 10.02  -9.3 10    18.05 27.2  35.6
    ##      tmin       0      366 366  9.91  9.18 -18.2  2.92  8.9  18.3  27.2
    ##      hist
    ##  ▇▁▁▁▁▁▁▁
    ##  ▁▂▅▆▇▆▇▆
    ##  ▁▁▃▇▇▅▇▅

write a function

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
}
```

``` r
weather_lm(weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
map(weather_nest$data, ~lm(tmax ~ tmin, data = .x))
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = .x)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

save output as a new list column

``` r
weather_nest %>% 
  mutate(lm_result = map(weather_nest$data, weather_lm))
```

    ## # A tibble: 3 x 4
    ##   name           id          data               lm_result
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [366 × 4]> <S3: lm> 
    ## 2 Waikiki_HA     USC00519397 <tibble [366 × 4]> <S3: lm> 
    ## 3 Waterhole_WA   USS0023B17S <tibble [366 × 4]> <S3: lm>
