# Using arules with tidyverse

`arules` works seamlessly with [tidyverse](https://tidyverse.org/). For
example:

- `dplyr` can be used for cleaning and preparing the transactions.
- [`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  and other functions accept `tibble` as input.
- Functions in arules can be connected with the pipe operator `|>`.
- [arulesViz](https://mhahsler.github.io/arulesViz) provides
  visualizations based on `ggplot2`.

For example, we can remove the ethnic information column before creating
transactions and then mine and inspect rules.

``` r

library("tidyverse")
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.2.1     ✔ readr     2.2.0
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
#> ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
#> ✔ purrr     1.2.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ tidyr::expand() masks Matrix::expand()
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ✖ tidyr::pack()   masks Matrix::pack()
#> ✖ dplyr::recode() masks arules::recode()
#> ✖ tidyr::unpack() masks Matrix::unpack()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library("arules")
data("IncomeESL")

trans <- IncomeESL |>
  select(-`ethnic classification`) |>
  transactions()
rules <- trans |>
  apriori(
    supp = 0.1, conf = 0.9, target = "rules",
    control = list(verbose = FALSE)
  )
rules |>
  head(3, by = "lift") |>
  as("data.frame") |>
  tibble()
#> # A tibble: 3 × 6
#>   rules                                  support confidence coverage  lift count
#>   <chr>                                    <dbl>      <dbl>    <dbl> <dbl> <int>
#> 1 {dual incomes=no,householder status=o…   0.102      0.971    0.105  2.62   914
#> 2 {years in bay area=>10,dual incomes=y…   0.100      0.961    0.104  2.59   902
#> 3 {dual incomes=yes,householder status=…   0.110      0.960    0.114  2.59   988
```
