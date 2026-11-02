assignmentB1
================
April Radford
2026-10-30

``` r
#import libraries
library(roxygen2)
library(testthat)
```

``` r
#' description: this function converts an area in hectares to any unit in the list: square kilometers, square metres, acres
#' 
#' @param area_ha : input area of hectares to be converted. name was chosen for clarity, as this sould be area in hectares.
#' @param to_unit : unit to be converted to, possible inputs are "km2", "m2", and "acres". name was chosen for clarity. it describes the purpose of the parameter.
#' @returns input area converted to chosen unit
convert_ha <- function(area_ha, to_unit = c("km2", "m2", "acres")){
  if(!is.numeric(area_ha)){
    stop("input must be numeric")}
  factors <- c(km2 = 0.01, m2 = 10000, acres = 2.47105)
  result <- area_ha * factors[[to_unit]]
  result}
```

``` r
#convert 100 hectares to square kilometres
convert_ha(100, "km2")
```

    ## [1] 1

``` r
#convert 100 hectares to square metres
convert_ha(100, "m2")
```

    ## [1] 1e+06

``` r
#convert 100 hectares to acres
convert_ha(100, "acres")
```

    ## [1] 247.105

``` r
#One hectare is equal to 2.47105 acres. This tests that teh conversion is accurate.
test_that("One hectare equals 2.47105 acres", {
  expect_equal(convert_ha(1, "acres"), 2.47105)
})
```

    ## Test passed 🥳

``` r
# test that negative inputs still work. In certain cases, a negative value may need to be converted as an area.
test_that("Negative areas are still calculated", {
  expect_equal(convert_ha(-100, "km2"), -1)
})
```

    ## Test passed 🎊

``` r
#test that the function still works on non-integers
test_that("Non-integer inputs work correctly", {
  inputs    <- c(0.5,   2.5,     10.75)
  expected  <- c(1.235525, 6.177625, 26.5637875)
  expect_equal(convert_ha(inputs, "acres"), expected)
})
```

    ## Test passed 🎊

``` r
#test that non-numeric inputs are not accepted
test_that("Non-numeric inputs are rejected", {
  expect_error(convert_ha("one", "acres"))
  expect_error(convert_ha("1", "acres"))
})
```

    ## Test passed 😸

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
