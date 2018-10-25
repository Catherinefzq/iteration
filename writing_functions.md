Untitled
================
Catherine
10/25/2018

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872
    ##  [6] -1.04107494  0.33550276  0.59957343  0.42849461 -0.49894708
    ## [11]  1.41364561  0.23279252 -0.83138529 -2.50852027  1.00648110
    ## [16] -0.22481531 -0.19456260  0.81587675  0.68682298  0.44756609
    ## [21]  0.78971253  0.64568566 -0.09904161 -2.27133861  0.47485186

#### Z score function

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  z
  
}

z_scores(x)
```

    ##  [1] -0.83687228  0.01576465 -1.05703126  1.50152998  0.16928872
    ##  [6] -1.04107494  0.33550276  0.59957343  0.42849461 -0.49894708
    ## [11]  1.41364561  0.23279252 -0.83138529 -2.50852027  1.00648110
    ## [16] -0.22481531 -0.19456260  0.81587675  0.68682298  0.44756609
    ## [21]  0.78971253  0.64568566 -0.09904161 -2.27133861  0.47485186

``` r
# z_scores(3)
# z_scores("my name is jeff")
# z_scores(iris)
# z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
```

Put in some checks in inputs

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = mean(x) / sd(x)
  
  z
}
```

``` r
# check some examples
#z_scores("my name is jeff")
```

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

mean_and_sd_list = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```

``` r
mean_and_sd(c(1,2,3))
```

    ## # A tibble: 1 x 2
    ##   mean_x  sd_x
    ##    <dbl> <dbl>
    ## 1      2     1

``` r
mean_and_sd_list(c(1,2,3))
```

    ## $mean
    ## [1] 2
    ## 
    ## $sd
    ## [1] 1
