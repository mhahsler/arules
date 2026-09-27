# Itemwise Set Operations

Provides the generic functions and the methods for itemwise set
operations on items in an
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).
The regular set operations regard each itemset in an `itemMatrix` as an
element. Itemwise operations regard each item as an element and operate
on the items of pairs of corresponding itemsets (first itemset in `x`
with first itemset in `y`, second with second, etc.).

## Usage

``` r
itemUnion(x, y)

itemSetdiff(x, y)

itemIntersect(x, y)

# S4 method for class 'itemMatrix,itemMatrix'
itemUnion(x, y)

# S4 method for class 'itemMatrix,itemMatrix'
itemSetdiff(x, y)

# S4 method for class 'itemMatrix,itemMatrix'
itemIntersect(x, y)
```

## Arguments

- x, y:

  two
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  objects with the same number of rows (itemsets).

## Value

An object of class
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
is returned.

## See also

Other itemMatrix and transactions functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
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
data("Adult")

fsets <- eclat(Adult, parameter = list(supp = 0.5))
#> Eclat
#> 
#> parameter specification:
#>  tidLists support minlen maxlen            target  ext
#>     FALSE     0.5      1     10 frequent itemsets TRUE
#> 
#> algorithmic control:
#>  sparse sort verbose
#>       7   -2    TRUE
#> 
#> Absolute minimum support count: 24421 
#> 
#> create itemset ... 
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [9 item(s)] done [0.00s].
#> creating bit matrix ... [9 row(s), 48842 column(s)] done [0.00s].
#> writing  ... [49 set(s)] done [0.00s].
#> Creating S4 object  ... done [0.00s].
inspect(fsets[1:4])
#>     items                            support count
#> [1] {capital-gain=None,                           
#>      capital-loss=None,                           
#>      hours-per-week=Full-time}     0.5191638 25357
#> [2] {capital-loss=None,                           
#>      hours-per-week=Full-time}     0.5606650 27384
#> [3] {capital-gain=None,                           
#>      hours-per-week=Full-time}     0.5435895 26550
#> [4] {hours-per-week=Full-time,                    
#>      native-country=United-States} 0.5179559 25298
inspect(itemUnion(items(fsets[1:2]), items(fsets[3:4])))
#>     items                         
#> [1] {capital-gain=None,           
#>      capital-loss=None,           
#>      hours-per-week=Full-time}    
#> [2] {capital-loss=None,           
#>      hours-per-week=Full-time,    
#>      native-country=United-States}
inspect(itemSetdiff(items(fsets[1:2]), items(fsets[3:4])))
#>     items              
#> [1] {capital-loss=None}
#> [2] {capital-loss=None}
inspect(itemIntersect(items(fsets[1:2]), items(fsets[3:4])))
#>     items                                        
#> [1] {capital-gain=None, hours-per-week=Full-time}
#> [2] {hours-per-week=Full-time}                   
```
