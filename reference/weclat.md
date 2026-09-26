# Mining Associations from Weighted Transaction Data with Eclat (WARM)

Find frequent
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
with the Eclat algorithm. This implementation uses optimized transaction
ID list joins and transaction weights to implement weighted association
rule mining (WARM).

## Usage

``` r
weclat(data, parameter = NULL, control = NULL)
```

## Arguments

- data:

  an object that can be coerced into an object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

- parameter:

  an object of class
  [ASparameter](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  (default values: `support = 0.1`, `minlen = 1L`, and `maxlen = 5L`) or
  a named list with corresponding components.

- control:

  an object of class
  [AScontrol](http://michael.hahsler.net/arules/reference/AScontrol-classes.md)
  (default values: `verbose = TRUE`) or a named list with corresponding
  components.

## Value

Returns an object of class
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md).
Note that weighted support is returned in
[quality](http://michael.hahsler.net/arules/reference/associations-class.md)
as column `support`.

## Details

Transaction weights are stored in the
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
as a column called `weight` in
[transactionInfo](http://michael.hahsler.net/arules/reference/transactions-class.md).

The weighted support of an itemset is the sum of the weights of the
transactions that contain the itemset. An itemset is frequent if its
weighted support is equal or greater than the threshold specified by
`support` (assuming that the weights sum to one).

Note that Eclat only mines (weighted) frequent itemsets. Weighted
association rules can be created using
[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md).

## Note

The C code can be interrupted by `CTRL-C`. This is convenient but comes
at the price that the code cannot clean up its internal memory.

## References

G.D. Ramkumar, S. Ranka, and S. Tsur (1998). Weighted Association Rules:
Model and Algorithm, *Proceedings of ACM SIGKDD.*

## See also

Other mining algorithms:
[`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md),
[`AScontrol-classes`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md),
[`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md),
[`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md),
[`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md),
[`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md),
[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)

Other weighted association mining functions:
[`SunBai`](http://michael.hahsler.net/arules/reference/SunBai.md),
[`hits()`](http://michael.hahsler.net/arules/reference/hits.md)

## Author

Christian Buchta

## Examples

``` r
## Example 1: SunBai data
data(SunBai)
SunBai
#> transactions in sparse format with
#>  6 transactions (rows) and
#>  8 items (columns)

## weights are stored in transactionInfo
transactionInfo(SunBai)
#>   transactionID    weight
#> 1           100 0.5176528
#> 2           200 0.4362571
#> 3           300 0.2321374
#> 4           400 0.1476262
#> 5           500 0.5440458
#> 6           600 0.4123691

## mine weighted support itemsets using transaction support in SunBai
s <- weclat(SunBai,
  parameter = list(support = 0.3),
  control = list(verbose = TRUE)
)
#> Weighted Eclat (WEclat)
#> 
#> parameter specification:
#>  support minlen maxlen target ext
#>      0.3      1     10   <NA>  NA
#> 
#> algorithmic control:
#>  sort verbose
#>    NA    TRUE
#> 
#> preparing ... 8 items, 6 L1 [0.00s]
#> mining ... 6 transactions, 0.33 used [0.00s]
#> writing ... 12 itemsets [0.00s]
inspect(sort(s))
#>      items     support  
#> [1]  {C}       0.6541039
#> [2]  {G}       0.6081302
#> [3]  {A}       0.5719366
#> [4]  {F}       0.4280634
#> [5]  {F, G}    0.4280634
#> [6]  {C, F, G} 0.4280634
#> [7]  {C, F}    0.4280634
#> [8]  {C, G}    0.4280634
#> [9]  {H}       0.4176323
#> [10] {G, H}    0.4176323
#> [11] {B}       0.3274066
#> [12] {A, B}    0.3274066

## create rules using weighted support (satisfying a minimum
## weighted confidence of 90%).
r <- ruleInduction(s, confidence = .9)
inspect(r)
#>     lhs       rhs support   confidence lift    
#> [1] {B}    => {A} 0.3274066 1          1.748445
#> [2] {H}    => {G} 0.4176323 1          1.644385
#> [3] {F}    => {G} 0.4280634 1          1.644385
#> [4] {F, G} => {C} 0.4280634 1          1.528809
#> [5] {C, G} => {F} 0.4280634 1          2.336103
#> [6] {C, F} => {G} 0.4280634 1          1.644385
#> [7] {F}    => {C} 0.4280634 1          1.528809

## Example 2: Find association rules in weighted data
trans <- list(
  c("A", "B", "C", "D", "E"),
  c("C", "F", "G"),
  c("A", "B"),
  c("A"),
  c("C", "F", "G", "H"),
  c("A", "G", "H")
)

weight <- c(5, 10, 6, 7, 5, 1)

## convert list to transactions
trans <- transactions(trans)

## add weight information
transactionInfo(trans) <- data.frame(weight = weight)
inspect(trans)
#>     items           weight
#> [1] {A, B, C, D, E}  5    
#> [2] {C, F, G}       10    
#> [3] {A, B}           6    
#> [4] {A}              7    
#> [5] {C, F, G, H}     5    
#> [6] {A, G, H}        1    

## mine weighed support itemsets
s <- weclat(trans,
  parameter = list(support = 0.3),
  control = list(verbose = TRUE)
)
#> Weighted Eclat (WEclat)
#> 
#> parameter specification:
#>  support minlen maxlen target ext
#>      0.3      1     10   <NA>  NA
#> 
#> algorithmic control:
#>  sort verbose
#>    NA    TRUE
#> 
#> preparing ... 8 items, 5 L1 [0.00s]
#> mining ... 6 transactions, 0.28 used [0.00s]
#> writing ... 10 itemsets [0.00s]
inspect(sort(s))
#>      items     support  
#> [1]  {C}       0.5882353
#> [2]  {A}       0.5588235
#> [3]  {G}       0.4705882
#> [4]  {F}       0.4411765
#> [5]  {F, G}    0.4411765
#> [6]  {C, F, G} 0.4411765
#> [7]  {C, F}    0.4411765
#> [8]  {C, G}    0.4411765
#> [9]  {B}       0.3235294
#> [10] {A, B}    0.3235294

## create association rules
r <- ruleInduction(s, confidence = .5)
inspect(r)
#>      lhs       rhs support   confidence lift    
#> [1]  {B}    => {A} 0.3235294 1.0000000  1.789474
#> [2]  {A}    => {B} 0.3235294 0.5789474  1.789474
#> [3]  {G}    => {F} 0.4411765 0.9375000  2.125000
#> [4]  {F}    => {G} 0.4411765 1.0000000  2.125000
#> [5]  {F, G} => {C} 0.4411765 1.0000000  1.700000
#> [6]  {C, G} => {F} 0.4411765 1.0000000  2.266667
#> [7]  {C, F} => {G} 0.4411765 1.0000000  2.125000
#> [8]  {F}    => {C} 0.4411765 1.0000000  1.700000
#> [9]  {C}    => {F} 0.4411765 0.7500000  1.700000
#> [10] {G}    => {C} 0.4411765 0.9375000  1.593750
#> [11] {C}    => {G} 0.4411765 0.7500000  1.593750
```
