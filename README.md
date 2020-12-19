
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geochem

<!-- badges: start -->

<!-- badges: end -->

\! Developement of this package has just begun \!

The goal of geochem is to analyse and wrangle geochemical data sets more
efficiently with the help of R. Most geochemical datasets contain
elements as variables and only a few categorical variable. Geochems aim
is it to make looking at those different elements in a more consistend
workflow.

## Installation

Package is not on [CRAN](https://CRAN.R-project.org) yet.

You can install the released version of geochem from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geochem")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("muhohl/geochem")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geochem)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!

### Laser Maps

TODO Write instructions for the laser maps function\!

1.  Load the files / Create a random laser map data frame

2.  Select the elements

3.  Create a Log\_Trans tibble

4.  Use the clipping function

5.  Clip the data set

6.  Create list with plots

7.  Plot the list with plots

8.  Save the plot

### XMOD Maps

A couple of functions that allow the user to plot XMOD maps.

# TODO

    * Add remaining functions. 
    * Create dummy data frames for laser spot data, a laser map, and a XMOD data set.
