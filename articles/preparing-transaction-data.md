# Preparing transaction data

Data for association rule mining comes from many sources and in several
layouts. `arules` stores these data in the sparse `transactions` class,
and the
[`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
constructor accepts several common input layouts.

The following examples show how to convert each layout. Always inspect
the resulting `transactions` object with
[`summary()`](https://rdrr.io/r/base/summary.html) or
[`itemLabels()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md):
values that were encoded incorrectly in the source data may otherwise
become unintended items.

## A list of baskets

Use one character vector per transaction. List names become transaction
IDs.

``` r

baskets <- list(
  order_1 = c("apple", "bread"),
  order_2 = c("bread", "milk"),
  order_3 = c("apple", "bread", "milk")
)
from_list <- transactions(baskets)
inspect(from_list)
#>     items                transactionID
#> [1] {apple, bread}       order_1      
#> [2] {bread, milk}        order_2      
#> [3] {apple, bread, milk} order_3
```

Check both the transaction summary and the resulting item labels.

``` r

summary(from_list)
#> transactions as itemMatrix in sparse format with
#>  3 rows (elements/itemsets/transactions) and
#>  3 columns (items) and a density of 0.7777778 
#> 
#> most frequent items:
#>   bread   apple    milk (Other) 
#>       3       2       2       0 
#> 
#> element (itemset/transaction) length distribution:
#> sizes
#> 2 3 
#> 2 1 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.000   2.000   2.000   2.333   2.500   3.000 
#> 
#> includes extended item information - examples:
#>   labels
#> 1  apple
#> 2  bread
#> 3   milk
#> 
#> includes extended transaction information - examples:
#>   transactionID
#> 1       order_1
#> 2       order_2
#> 3       order_3
itemLabels(from_list)
#> [1] "apple" "bread" "milk"
```

The item labels confirm that the baskets were translated correctly.

## A binary matrix

Rows represent transactions and columns represent items. Logical
matrices make the intended coding explicit.

``` r

binary <- matrix(
  c(TRUE, TRUE, FALSE,
    FALSE, TRUE, TRUE,
    TRUE, TRUE, TRUE),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(names(baskets), c("apple", "bread", "milk"))
)
from_matrix <- transactions(binary)

itemLabels(from_matrix)
#> [1] "apple" "bread" "milk"
inspect(from_matrix)
#>     items                transactionID
#> [1] {apple, bread}       order_1      
#> [2] {bread, milk}        order_2      
#> [3] {apple, bread, milk} order_3
```

## A data frame in wide format

Categorical columns are converted to items of the form `variable=value`.
Logical columns represent the presence or absence of a single item.
Missing values are omitted.

``` r

customers <- data.frame(
  age_group = factor(c("young", "adult", "adult")),
  region = factor(c("north", "south", "north")),
  subscriber = c(TRUE, FALSE, TRUE)
)
from_wide <- transactions(customers)

itemLabels(from_wide)
#> [1] "age_group=adult" "age_group=young" "region=north"    "region=south"   
#> [5] "subscriber"
inspect(from_wide)
#>     items                                       transactionID
#> [1] {age_group=young, region=north, subscriber} 1            
#> [2] {age_group=adult, region=south}             2            
#> [3] {age_group=adult, region=north, subscriber} 3
```

Continuous variables need to be discretized before conversion.

``` r

measurements <- data.frame(
  spend = c(12, 18, 35, 42, 55),
  visits = c(1, 2, 3, 5, 8)
)
measurements_discrete <- discretizeDF(
  measurements,
  default = list(method = "frequency", breaks = 2)
)
from_discrete <- transactions(measurements_discrete)

itemLabels(from_discrete)
#> [1] "spend=[12,35)" "spend=[35,55]" "visits=[1,3)"  "visits=[3,8]"
inspect(from_discrete)
#>     items                         transactionID
#> [1] {spend=[12,35), visits=[1,3)} 1            
#> [2] {spend=[12,35), visits=[1,3)} 2            
#> [3] {spend=[35,55], visits=[3,8]} 3            
#> [4] {spend=[35,55], visits=[3,8]} 4            
#> [5] {spend=[35,55], visits=[3,8]} 5
```

## A data frame in long format

Long-format data has one row per transaction–item pair. Identify the
transaction and item columns with `cols`.

``` r

long <- data.frame(
  order = c(1, 1, 2, 2, 3),
  product = c("apple", "bread", "bread", "milk", "apple")
)
from_long <- transactions(long, format = "long", cols = c("order", "product"))

itemLabels(from_long)
#> [1] "apple" "bread" "milk"
inspect(from_long)
#>     items          transactionID
#> [1] {apple, bread} 1            
#> [2] {bread, milk}  2            
#> [3] {apple}        3
```

## Other vignettes

- [Getting started with
  arules](http://michael.hahsler.net/arules/articles/getting-started.md)
- [Mining and pruning association
  rules](http://michael.hahsler.net/arules/articles/mining-and-pruning-rules.md)
- [Interest
  measures](http://michael.hahsler.net/arules/articles/interest-measures.md)
- [Item
  hierarchies](http://michael.hahsler.net/arules/articles/item-hierarchies.md)
