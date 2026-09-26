# The Income Data Set

Survey example data from the book *The Elements of Statistical
Learning*.

## Format

The data is provided in two formats:

1.  `Income` is an object of class
    [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
    with 6876 transactions (complete cases) and 50 items. See below for
    details.

2.  `IncomeESL` is a data frame with 8993 observations on the following
    14 variables:

- income:

  an ordered factor with levels `[0,10)` \< `[10,15)` \< `[15,20)` \<
  `[20,25)` \< `[25,30)` \< `[30,40)` \< `[40,50)` \< `[50,75)` \< `75+`

- sex:

  a factor with levels `male` `female`

- marital status:

  a factor with levels `married` `cohabitation` `divorced` `widowed`
  `single`

- age:

  an ordered factor with levels `14-17` \< `18-24` \< `25-34` \< `35-44`
  \< `45-54` \< `55-64` \< `65+`

- education:

  an ordered factor with levels `grade <9` \< `grades 9-11` \<
  `high school graduate` \< `college (1-3 years)` \< `college graduate`
  \< `graduate study`

- occupation:

  a factor with levels `professional/managerial` `sales` `laborer`
  `clerical/service` `homemaker` `student` `military` `retired`
  `unemployed`

- years in bay area:

  an ordered factor with levels `<1` \< `1-3` \< `4-6` \< `7-10` \<
  `>10`

- dual incomes:

  a factor with levels `not married` `yes` `no`

- number in household:

  an ordered factor with levels `1` \< `2` \< `3` \< `4` \< `5` \< `6`
  \< `7` \< `8` \< `9+`

- number of children:

  an ordered factor with levels `0` \< `1` \< `2` \< `3` \< `4` \< `5`
  \< `6` \< `7` \< `8` \< `9+`

- householder status:

  a factor with levels `own` `rent` `live with parents/family`

- type of home:

  a factor with levels `house` `condominium` `apartment` `mobile Home`
  `other`

- ethnic classification:

  a factor with levels `american indian` `asian` `black` `east indian`
  `hispanic` `pacific islander` `white` `other`

- language in home:

  a factor with levels `english` `spanish` `other`

## Source

Impact Resources, Inc., Columbus, OH (1987).

Obtained from the web site of the book: Hastie, T., Tibshirani, R. &
Friedman, J. (2001) *The Elements of Statistical Learning*.
Springer-Verlag.

## Details

The `IncomeESL` data set originates from an example in the book *The
Elements of Statistical Learning* (see Section source). The data set is
an extract from this survey. It consists of 8993 instances (obtained
from the original data set with 9409 instances, by removing those
observations with the annual income missing) with 14 demographic
attributes. The data set is a good mixture of categorical and continuous
variables with a lot of missing data. This is characteristic of data
mining applications. The Income data set contains the data already
prepared and coerced to
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

To create transactions for Income, the original data frame in
`IncomeESL` is prepared in a similar way as described in *The Elements
of Statistical Learning.* We removed cases with missing values and cut
each ordinal variable (age, education, income, years in bay area, number
in household, and number of children) at its median into two values (see
Section examples).

## Author

Michael Hahsler

## Examples

``` r
data("IncomeESL")
IncomeESL[1:3, ]
#>   income    sex marital status   age           education
#> 1    75+ female        married 45-54 college (1-3 years)
#> 2    75+   male        married 45-54    college graduate
#> 3    75+ female        married 25-34    college graduate
#>                occupation years in bay area dual incomes number in household
#> 1               homemaker               >10           no                   3
#> 2               homemaker               >10           no                   5
#> 3 professional/managerial               >10          yes                   3
#>   number of children householder status type of home ethnic classification
#> 1                  0                own        house                 white
#> 2                  2                own        house                 white
#> 3                  1               rent    apartment                 white
#>   language in home
#> 1             <NA>
#> 2          english
#> 3          english

## remove incomplete cases
IncomeESL <- IncomeESL[complete.cases(IncomeESL), ]

## preparing the data set
IncomeESL[["income"]] <- factor((as.numeric(IncomeESL[["income"]]) > 6) + 1,
  levels = 1:2, labels = c("$0-$40,000", "$40,000+")
)

IncomeESL[["age"]] <- factor((as.numeric(IncomeESL[["age"]]) > 3) + 1,
  levels = 1:2, labels = c("14-34", "35+")
)

IncomeESL[["education"]] <- factor((as.numeric(IncomeESL[["education"]]) > 4) + 1,
  levels = 1:2, labels = c("no college graduate", "college graduate")
)

IncomeESL[["years in bay area"]] <- factor(
  (as.numeric(IncomeESL[["years in bay area"]]) > 4) + 1,
  levels = 1:2, labels = c("1-9", "10+")
)

IncomeESL[["number in household"]] <- factor(
  (as.numeric(IncomeESL[["number in household"]]) > 3) + 1,
  levels = 1:2, labels = c("1", "2+")
)

IncomeESL[["number of children"]] <- factor(
  (as.numeric(IncomeESL[["number of children"]]) > 1) + 0,
  levels = 0:1, labels = c("0", "1+")
)

##  creating transactions
Income <- transactions(IncomeESL)
Income
#> transactions in sparse format with
#>  6876 transactions (rows) and
#>  50 items (columns)
```
