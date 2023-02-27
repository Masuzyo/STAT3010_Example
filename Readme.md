``` r
library(tidyverse)
library(psych)
library(knitr)
library(cowplot)
library(GGally)
library(rstatix)
```

# Introduction

<figure>
<img
src="http://www.phytoimages.siu.edu/users/Cusman1/5_30_14/DSCF0206Irissetosa.jpg"
style="width:25.0%" alt="Setosa" />
<figcaption aria-hidden="true">Setosa</figcaption>
</figure>

In this project we are going to try and determine the physical
differences between three sub-species of Iris using measurement from
their flowers. The both length and width of the sepal and the petal of
50 flowers of each species was measured and record in this study.

# Data Structure and Visualisation

### General summary

``` r
kable(summary(iris)) #overall summary
```

|     | Sepal.Length  | Sepal.Width   | Petal.Length  | Petal.Width   | Species       |
|:---|:-------------|:-------------|:-------------|:-------------|:-------------|
|     | Min. :4.300   | Min. :2.000   | Min. :1.000   | Min. :0.100   | setosa :50    |
|     | 1st Qu.:5.100 | 1st Qu.:2.800 | 1st Qu.:1.600 | 1st Qu.:0.300 | versicolor:50 |
|     | Median :5.800 | Median :3.000 | Median :4.350 | Median :1.300 | virginica :50 |
|     | Mean :5.843   | Mean :3.057   | Mean :3.758   | Mean :1.199   | NA            |
|     | 3rd Qu.:6.400 | 3rd Qu.:3.300 | 3rd Qu.:5.100 | 3rd Qu.:1.800 | NA            |
|     | Max. :7.900   | Max. :4.400   | Max. :6.900   | Max. :2.500   | NA            |

``` r
db <- describeBy(iris,group=iris$Species) #Summaries by Group
```

## Summary by Species

### Setosa

``` r
kable(db$setosa)
```

|              | vars |   n |  mean |        sd | median | trimmed |     mad | min | max | range |      skew |   kurtosis |        se |
|:--------|---:|--:|----:|------:|----:|-----:|-----:|---:|---:|----:|------:|------:|------:|
| Sepal.Length |    1 |  50 | 5.006 | 0.3524897 |    5.0 |  5.0025 | 0.29652 | 4.3 | 5.8 |   1.5 | 0.1129778 | -0.4508724 | 0.0498496 |
| Sepal.Width  |    2 |  50 | 3.428 | 0.3790644 |    3.4 |  3.4150 | 0.37065 | 2.3 | 4.4 |   2.1 | 0.0387295 |  0.5959507 | 0.0536078 |
| Petal.Length |    3 |  50 | 1.462 | 0.1736640 |    1.5 |  1.4600 | 0.14826 | 1.0 | 1.9 |   0.9 | 0.1000954 |  0.6539303 | 0.0245598 |
| Petal.Width  |    4 |  50 | 0.246 | 0.1053856 |    0.2 |  0.2375 | 0.00000 | 0.1 | 0.6 |   0.5 | 1.1796328 |  1.2587179 | 0.0149038 |
| Species\*    |    5 |  50 | 1.000 | 0.0000000 |    1.0 |  1.0000 | 0.00000 | 1.0 | 1.0 |   0.0 |       NaN |        NaN | 0.0000000 |

### Versicolor

``` r
kable(db$versicolor)
```

|              | vars |   n |  mean |        sd | median | trimmed |     mad | min | max | range |       skew |   kurtosis |        se |
|:-------|---:|--:|----:|------:|----:|-----:|-----:|---:|---:|----:|------:|------:|------:|
| Sepal.Length |    1 |  50 | 5.936 | 0.5161711 |   5.90 |  5.9375 | 0.51891 | 4.9 | 7.0 |   2.1 |  0.0991393 | -0.6939138 | 0.0729976 |
| Sepal.Width  |    2 |  50 | 2.770 | 0.3137983 |   2.80 |  2.7800 | 0.29652 | 2.0 | 3.4 |   1.4 | -0.3413644 | -0.5493203 | 0.0443778 |
| Petal.Length |    3 |  50 | 4.260 | 0.4699110 |   4.35 |  4.2925 | 0.51891 | 3.0 | 5.1 |   2.1 | -0.5706024 | -0.1902555 | 0.0664554 |
| Petal.Width  |    4 |  50 | 1.326 | 0.1977527 |   1.30 |  1.3250 | 0.22239 | 1.0 | 1.8 |   0.8 | -0.0293338 | -0.5873144 | 0.0279665 |
| Species\*    |    5 |  50 | 2.000 | 0.0000000 |   2.00 |  2.0000 | 0.00000 | 2.0 | 2.0 |   0.0 |        NaN |        NaN | 0.0000000 |

### Virginica

``` r
kable(db$virginica)
```

|              | vars |   n |  mean |        sd | median | trimmed |     mad | min | max | range |       skew |   kurtosis |        se |
|:-------|---:|--:|----:|------:|----:|-----:|-----:|---:|---:|----:|------:|------:|------:|
| Sepal.Length |    1 |  50 | 6.588 | 0.6358796 |   6.50 |  6.5725 | 0.59304 | 4.9 | 7.9 |   3.0 |  0.1110286 | -0.2032597 | 0.0899270 |
| Sepal.Width  |    2 |  50 | 2.974 | 0.3224966 |   3.00 |  2.9625 | 0.29652 | 2.2 | 3.8 |   1.6 |  0.3442849 |  0.3803832 | 0.0456079 |
| Petal.Length |    3 |  50 | 5.552 | 0.5518947 |   5.55 |  5.5100 | 0.66717 | 4.5 | 6.9 |   2.4 |  0.5169175 | -0.3651161 | 0.0780497 |
| Petal.Width  |    4 |  50 | 2.026 | 0.2746501 |   2.00 |  2.0325 | 0.29652 | 1.4 | 2.5 |   1.1 | -0.1218119 | -0.7539586 | 0.0388414 |
| Species\*    |    5 |  50 | 3.000 | 0.0000000 |   3.00 |  3.0000 | 0.00000 | 3.0 | 3.0 |   0.0 |        NaN |        NaN | 0.0000000 |

## Boxplots

``` r
p <- ggplot(iris,aes(x=Species,fill=Species))
p1 <- p+geom_boxplot(aes(y=Sepal.Length))+ theme(legend.position = "none") 
p2 <- p+geom_boxplot(aes(y=Sepal.Width))+ theme(legend.position = "none") 
p3 <- p+geom_boxplot(aes(y=Petal.Length))+ theme(legend.position = "none") 
p4 <- p+geom_boxplot(aes(y=Petal.Width))+ theme(legend.position = "none") 
plot_grid(p1,p2,p3,p4)
```

![](Example_files/figure-markdown_github/Boxplots-1.png)

From the boxplots we can see there are clear difference between the
species based on several variables. This is strong evidence that we will
be able to find significant difference the groups

## Scatter plots

``` r
q <- ggplot(iris,aes(colour=Species))
q1 <-q+geom_point(aes(x=Sepal.Length,y=Sepal.Width))+labs(title="Sepal Length vs Sepal Width")+theme(legend.position = 'None')
q2 <-q+geom_point(aes(x=Petal.Length,y=Petal.Width))+labs(title="Petal Length vs Petal Width")+theme(legend.position = 'None')
plot_grid(q1,q2)
```

![](Example_files/figure-markdown_github/unnamed-chunk-6-1.png)

## Overall Summary

``` r
ggpairs(iris,aes(colour=Species))
```

![](Example_files/figure-markdown_github/unnamed-chunk-7-1.png)

# If we only had one? ANOVA

We wish to find the measurement that is most different among our given
Species. So will use Anova to determine if there are differences in the
measures for each Species. But first we check some the assumptions of
the anova.

## Test for Normality

``` r
iris %>% group_by(Species) %>% shapiro_test(Sepal.Width) # test for normality in each group
```

    ## # A tibble: 3 × 4
    ##   Species    variable    statistic     p
    ##   <fct>      <chr>           <dbl> <dbl>
    ## 1 setosa     Sepal.Width     0.972 0.272
    ## 2 versicolor Sepal.Width     0.974 0.338
    ## 3 virginica  Sepal.Width     0.967 0.181

Since our p-values are greater the *α* = 0.05 we fail to reject the null
hypothesis and proceed with the assumption of normality.

``` r
g_iris<- iris %>% group_by(Species)  
g_iris %>% shapiro_test(Sepal.Length)# test for normality in each group sepal length
```

    ## # A tibble: 3 × 4
    ##   Species    variable     statistic     p
    ##   <fct>      <chr>            <dbl> <dbl>
    ## 1 setosa     Sepal.Length     0.978 0.460
    ## 2 versicolor Sepal.Length     0.978 0.465
    ## 3 virginica  Sepal.Length     0.971 0.258

``` r
g_iris %>% shapiro_test(Petal.Length)# test for normality in each group sepal length
```

    ## # A tibble: 3 × 4
    ##   Species    variable     statistic      p
    ##   <fct>      <chr>            <dbl>  <dbl>
    ## 1 setosa     Petal.Length     0.955 0.0548
    ## 2 versicolor Petal.Length     0.966 0.158 
    ## 3 virginica  Petal.Length     0.962 0.110

``` r
g_iris %>% shapiro_test(Petal.Width)# test for normality in each group sepal length
```

    ## # A tibble: 3 × 4
    ##   Species    variable    statistic           p
    ##   <fct>      <chr>           <dbl>       <dbl>
    ## 1 setosa     Petal.Width     0.800 0.000000866
    ## 2 versicolor Petal.Width     0.948 0.0273     
    ## 3 virginica  Petal.Width     0.960 0.0870

Here we find that the normality assumption is only violated for one of
our groups. Since the violation is only in one group we will proceed.

## Test for Equal Variance

``` r
bartlett.test(Sepal.Width~Species,iris)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  Sepal.Width by Species
    ## Bartlett's K-squared = 2.0911, df = 2, p-value = 0.3515

Since our p-value\<*α* we fail to reject the null and conclude that our
variance are not significantly different from each other.

``` r
bartlett.test(Sepal.Length~Species,iris)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  Sepal.Length by Species
    ## Bartlett's K-squared = 16.006, df = 2, p-value = 0.0003345

``` r
bartlett.test(Petal.Width~Species,iris)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  Petal.Width by Species
    ## Bartlett's K-squared = 39.213, df = 2, p-value = 3.055e-09

``` r
bartlett.test(Petal.Length~Species,iris)
```

    ## 
    ##  Bartlett test of homogeneity of variances
    ## 
    ## data:  Petal.Length by Species
    ## Bartlett's K-squared = 55.423, df = 2, p-value = 9.229e-13

He we see heavy violations of the equality of variance since our
*n*<sub>*i*</sub> large we can proceed with our anova.

## ANOVA

``` r
summary(aov(Sepal.Width~Species,data=iris))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## Species       2  11.35   5.672   49.16 <2e-16 ***
    ## Residuals   147  16.96   0.115                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Our anova show us that there are statistically significant difference
between our groups

``` r
summary(aov(Sepal.Length~Species,data=iris))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## Species       2  63.21  31.606   119.3 <2e-16 ***
    ## Residuals   147  38.96   0.265                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(Petal.Length~Species,data=iris))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## Species       2  437.1  218.55    1180 <2e-16 ***
    ## Residuals   147   27.2    0.19                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(aov(Petal.Width~Species,data=iris))
```

    ##              Df Sum Sq Mean Sq F value Pr(>F)    
    ## Species       2  80.41   40.21     960 <2e-16 ***
    ## Residuals   147   6.16    0.04                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Just as we suspected when we observed the boxplots all our variable have
difference between groups. We therefore proceed to perform multiple
comparison testing.

## Multiple Comparison

``` r
TukeyHSD(aov(Sepal.Width~Species,data=iris),conf.level = .95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Sepal.Width ~ Species, data = iris)
    ## 
    ## $Species
    ##                        diff         lwr        upr     p adj
    ## versicolor-setosa    -0.658 -0.81885528 -0.4971447 0.0000000
    ## virginica-setosa     -0.454 -0.61485528 -0.2931447 0.0000000
    ## virginica-versicolor  0.204  0.04314472  0.3648553 0.0087802

``` r
TukeyHSD(aov(Sepal.Length~Species,data=iris),conf.level = .95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Sepal.Length ~ Species, data = iris)
    ## 
    ## $Species
    ##                       diff       lwr       upr p adj
    ## versicolor-setosa    0.930 0.6862273 1.1737727     0
    ## virginica-setosa     1.582 1.3382273 1.8257727     0
    ## virginica-versicolor 0.652 0.4082273 0.8957727     0

``` r
TukeyHSD(aov(Petal.Length~Species,data=iris),conf.level = .95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Petal.Length ~ Species, data = iris)
    ## 
    ## $Species
    ##                       diff     lwr     upr p adj
    ## versicolor-setosa    2.798 2.59422 3.00178     0
    ## virginica-setosa     4.090 3.88622 4.29378     0
    ## virginica-versicolor 1.292 1.08822 1.49578     0

``` r
TukeyHSD(aov(Petal.Width~Species,data=iris),conf.level = .95)
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = Petal.Width ~ Species, data = iris)
    ## 
    ## $Species
    ##                      diff       lwr       upr p adj
    ## versicolor-setosa    1.08 0.9830903 1.1769097     0
    ## virginica-setosa     1.78 1.6830903 1.8769097     0
    ## virginica-versicolor 0.70 0.6030903 0.7969097     0

In fact we find that each variable can be distinguished by their species
hence we should be able tell which species or flower is by looking at
any of the variables, but since petal length has the largest mean
difference it would be the easiest variable to use.

# Conclusion

In our investigation we sort to determine difference between species of
iris through a looking at measurements taken from their flower. We found
difference with respect to very measurement that look at but we found
the largest difference to be found within Petal Length. Future work
could look at build a model that predicts the species based on the given
measurements and as our investigation shows there are significant
differences between species based on these measurements.
