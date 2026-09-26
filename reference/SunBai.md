# The SunBai Weighted Transactions Data Set

A small example database for weighted association rule mining provided
as an object of class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

## Format

Object of class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
with 6 transactions and 8 items. Weights are stored as transaction
information.

## Source

K. Sun and F. Bai (2008). Mining Weighted Association Rules without
Preassigned Weights. *IEEE Transactions on Knowledge and Data
Engineering*, 4 (30), 489–495.

## Details

The data set contains the example database described in the paper by K.
Sun and F.Bai for illustration of the concepts of weighted association
rule mining. `weight` stored as transaction information denotes the
transaction weights obtained using the HITS algorithm.

## See also

Other weighted association mining functions:
[`hits()`](http://michael.hahsler.net/arules/reference/hits.md),
[`weclat()`](http://michael.hahsler.net/arules/reference/weclat.md)

## Examples

``` r
data(SunBai)
summary(SunBai)
#> transactions as itemMatrix in sparse format with
#>  6 rows (elements/itemsets/transactions) and
#>  8 columns (items) and a density of 0.375 
#> 
#> most frequent items:
#>       A       C       G       B       F (Other) 
#>       4       3       3       2       2       4 
#> 
#> element (itemset/transaction) length distribution:
#> sizes
#> 1 2 3 4 5 
#> 1 1 2 1 1 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    1.00    2.25    3.00    3.00    3.75    5.00 
#> 
#> includes extended item information - examples:
#>   labels
#> 1      A
#> 2      B
#> 3      C
#> 
#> includes extended transaction information - examples:
#>   transactionID    weight
#> 1           100 0.5176528
#> 2           200 0.4362571
#> 3           300 0.2321374
inspect(SunBai)
#>     items           transactionID weight   
#> [1] {A, B, C, D, E} 100           0.5176528
#> [2] {C, F, G}       200           0.4362571
#> [3] {A, B}          300           0.2321374
#> [4] {A}             400           0.1476262
#> [5] {C, F, G, H}    500           0.5440458
#> [6] {A, G, H}       600           0.4123691

transactionInfo(SunBai)
#>   transactionID    weight
#> 1           100 0.5176528
#> 2           200 0.4362571
#> 3           300 0.2321374
#> 4           400 0.1476262
#> 5           500 0.5440458
#> 6           600 0.4123691
```
