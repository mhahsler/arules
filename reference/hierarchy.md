# Support for Item Hierarchies

Functions to use item hierarchies to aggregate items at different group
levels, to perform multi-level transaction analysis.

## Usage

``` r
addAggregate(x, by, postfix = "*")

filterAggregate(x)

aggregate(x, ...)

# S4 method for class 'itemMatrix'
aggregate(x, by)

# S4 method for class 'itemsets'
aggregate(x, by)

# S4 method for class 'rules'
aggregate(x, by)
```

## Arguments

- x:

  a
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
  or [rules](http://michael.hahsler.net/arules/reference/rules-class.md)
  object.

- by:

  name of a field (hierarchy level) available in
  [itemInfo](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  of `x` or a grouping vector of the same length as the number of items
  in `x`. Items with the same group label in `by` are aggregated into a
  single item with that label. Note that the grouping vector will be
  coerced to factor before use.

- postfix:

  characters added to mark group-level items.

- ...:

  further arguments.

## Value

`aggregate()` returns an object of the same class as `x` encoded with a
number of items equal to the number of unique values in `by`. Note that
for associations (itemsets and rules) the number of associations in the
returned set will most likely be reduced since several associations
might map to the same aggregated association. `aggregate()` returns only
unique associations. Quality measures are removed because they are
generally invalid after aggregation; aggregate the transactions and mine
them again to obtain valid quality measures.

`addAggregate()` returns a new transactions object with the original
items and the group items added. `filterAggregate()` removes
associations containing both an item and its aggregate.

## Details

Often an item hierarchy is available for
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
used for association rule mining. For example, in a supermarket dataset
items like "bread" and "bagel" might belong to the item group (category)
"baked goods."

Transactions can store item hierarchies as additional columns in the
itemInfo data.frame (`"labels"` cannot be used since it is reserved for
the item labels).

**Aggregation:** To perform analysis at a group level of the item
hierarchy, `aggregate()` produces a new object with items aggregated to
a given group level. A group-level item is present if one or more of the
items in the group are present in the original object. If rules are
aggregated, and the aggregation would lead to the same aggregated group
item in the lhs and in the rhs, then that group item is removed from the
lhs. Rules or itemsets, which are not unique after the aggregation, are
also removed. Note also that the quality measures are not applicable to
the new rules and thus are removed. If these measures are required, then
aggregate the transactions before mining rules.

**Multi-level analysis:** To analyze relationships between individual
items and item groups at the same time, `addAggregate()` can be used to
create a new transactions object which contains both, the original items
and group-level items (marked with a given postfix). In association rule
mining, all items are handled the same, which means that we will produce
a large number of rules of the type:

`item A => group of item A`

with a confidence of 1. This will also happen if you mine itemsets.
`filterAggregate()` can be used to filter these spurious rules or
itemsets.

## See also

Other preprocessing:
[`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md),
[`itemCoding`](http://michael.hahsler.net/arules/reference/itemCoding.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md)

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
[`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md),
[`itemMatrix-class`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`merge()`](http://michael.hahsler.net/arules/reference/merge.md),
[`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md),
[`tidLists-class`](http://michael.hahsler.net/arules/reference/tidLists-class.md),
[`transactions-class`](http://michael.hahsler.net/arules/reference/transactions-class.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data("Groceries")
Groceries
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  169 items (columns)

## Groceries contains a hierarchy stored in itemInfo
head(itemInfo(Groceries))
#>              labels  level2           level1
#> 1       frankfurter sausage meat and sausage
#> 2           sausage sausage meat and sausage
#> 3        liver loaf sausage meat and sausage
#> 4               ham sausage meat and sausage
#> 5              meat sausage meat and sausage
#> 6 finished products sausage meat and sausage

## Example 1: Aggregate items using an existing hierarchy stored in itemInfo.
## We aggregate to level2 stored in Groceries. All items with the same level2 label
## will become a single item with that name.
## Note that the number of items is therefore reduced to 55
Groceries_level2 <- aggregate(Groceries, by = "level2")
Groceries_level2
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  55 items (columns)
head(itemInfo(Groceries_level2)) ## labels are alphabetically sorted!
#>             labels
#> 1        baby food
#> 2             bags
#> 3  bakery improver
#> 4 bathroom cleaner
#> 5             beef
#> 6             beer


## compare original and aggregated transactions
inspect(head(Groceries, 2))
#>     items                 
#> [1] {citrus fruit,        
#>      semi-finished bread, 
#>      margarine,           
#>      ready soups}         
#> [2] {tropical fruit,      
#>      yogurt,              
#>      coffee}              
inspect(head(Groceries_level2, 2))
#>     items                    
#> [1] {bread and backed goods, 
#>      fruit,                  
#>      soups/sauces,           
#>      vinegar/oils}           
#> [2] {coffee,                 
#>      dairy produce,          
#>      fruit}                  

## Example 2: Aggregate using a character vector.
## We create here labels manually to organize items by their first letter.
mylevels <- toupper(substr(itemLabels(Groceries), 1, 1))
head(mylevels)
#> [1] "F" "S" "L" "H" "M" "F"

Groceries_alpha <- aggregate(Groceries, by = mylevels)
Groceries_alpha
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  23 items (columns)
inspect(head(Groceries_alpha, 2))
#>     items       
#> [1] {C, M, R, S}
#> [2] {C, T, Y}   

## Example 3: Aggregate rules
## Note: You could also directly mine rules from aggregated transactions to
## get support, confidence, and lift
rules <- apriori(Groceries, parameter = list(supp = 0.005, conf = 0.5))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.5    0.1    1 none FALSE            TRUE       5   0.005      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 49 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
#> sorting and recoding items ... [120 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [120 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
rules
#> set of 120 rules 
inspect(rules[1])
#>     lhs                rhs          support     confidence coverage   lift    
#> [1] {baking powder} => {whole milk} 0.009252669 0.5229885  0.01769192 2.046793
#>     count
#> [1] 91   

rules_level2 <- aggregate(rules, by = "level2")
inspect(rules_level2[1])
#>     lhs                  rhs            
#> [1] {bakery improver} => {dairy produce}

## Example 4: Mine multi-level rules.
## (1) Add aggregate items. These items will have labels ending with a *
Groceries_multilevel <- addAggregate(Groceries, "level2")
summary(Groceries_multilevel)
#> transactions as itemMatrix in sparse format with
#>  9835 rows (elements/itemsets/transactions) and
#>  224 columns (items) and a density of 0.03652589 
#> 
#> most frequent items:
#>          dairy produce* bread and backed goods*        non-alc. drinks* 
#>                    4357                    3398                    3127 
#>             vegetables*              whole milk                 (Other) 
#>                    2685                    2513                   64388 
#> 
#> element (itemset/transaction) length distribution:
#> sizes
#>    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17 
#> 2159  151 1503  234 1094  297  736  320  594  301  376  272  330  218  212  173 
#>   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33 
#>  163  118  102   89   52   62   49   45   35   31   23   19    8   12   11    6 
#>   34   35   36   37   38   39   40   41   42   47   48   49 
#>    6    8    5    6    2    3    2    1    2    3    1    1 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.000   4.000   6.000   8.182  11.000  49.000 
#> 
#> includes extended item information - examples:
#>        labels  level2           level1 aggregatedBy aggregateLevels aggregateID
#> 1 frankfurter sausage meat and sausage         <NA>               1         213
#> 2     sausage sausage meat and sausage         <NA>               1         213
#> 3  liver loaf sausage meat and sausage         <NA>               1         213
inspect(head(Groceries_multilevel))
#>     items                        
#> [1] {citrus fruit,               
#>      semi-finished bread,        
#>      margarine,                  
#>      ready soups,                
#>      bread and backed goods*,    
#>      fruit*,                     
#>      soups/sauces*,              
#>      vinegar/oils*}              
#> [2] {tropical fruit,             
#>      yogurt,                     
#>      coffee,                     
#>      coffee*,                    
#>      dairy produce*,             
#>      fruit*}                     
#> [3] {whole milk,                 
#>      dairy produce*}             
#> [4] {pip fruit,                  
#>      yogurt,                     
#>      cream cheese ,              
#>      meat spreads,               
#>      cheese*,                    
#>      dairy produce*,             
#>      fruit*,                     
#>      meat spreads*}              
#> [5] {other vegetables,           
#>      whole milk,                 
#>      condensed milk,             
#>      long life bakery product,   
#>      dairy produce*,             
#>      long-life bakery products*, 
#>      shelf-stable dairy*,        
#>      vegetables*}                
#> [6] {whole milk,                 
#>      butter,                     
#>      yogurt,                     
#>      rice,                       
#>      abrasive cleaner,           
#>      cleaner*,                   
#>      dairy produce*,             
#>      staple foods*}              

rules <- apriori(Groceries_multilevel,
  parameter = list(support = 0.01, conf = .9)
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.9    0.1    1 none FALSE            TRUE       5    0.01      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 98 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[224 item(s), 9835 transaction(s)] done [0.00s].
#> sorting and recoding items ... [132 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 done [0.01s].
#> writing ... [3649 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
inspect(head(rules, by = "lift"))
#>     lhs                             rhs                             support confidence   coverage     lift count
#> [1] {packaged fruit/vegetables*} => {packaged fruit/vegetables}  0.01301474          1 0.01301474 76.83594   128
#> [2] {packaged fruit/vegetables}  => {packaged fruit/vegetables*} 0.01301474          1 0.01301474 76.83594   128
#> [3] {seasonal products}          => {seasonal products*}         0.01423488          1 0.01423488 70.25000   140
#> [4] {seasonal products*}         => {seasonal products}          0.01423488          1 0.01423488 70.25000   140
#> [5] {canned fish*}               => {canned fish}                0.01504830          1 0.01504830 66.45270   148
#> [6] {canned fish}                => {canned fish*}               0.01504830          1 0.01504830 66.45270   148
## Note that this contains many spurious rules of type 'item X => aggregate of item X'
## with a confidence of 1 and high lift. We can filter spurious rules resulting from
## the aggregation
rules <- filterAggregate(rules)
inspect(head(rules, by = "lift"))
#>     lhs                           rhs                 support confidence   coverage     lift count
#> [1] {other vegetables,                                                                            
#>      bread and backed goods*,                                                                     
#>      cheese*,                                                                                     
#>      fruit*}                   => {dairy produce*} 0.01260803  0.9051095 0.01392984 2.043092   124
```
