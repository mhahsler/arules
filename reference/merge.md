# Adding Items to Data

Provides the generic function `merge()` and the methods for
[itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
and
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
to add new items to existing data.

## Usage

``` r
merge(x, y, ...)

# S4 method for class 'itemMatrix'
merge(x, y, ...)

# S4 method for class 'transactions'
merge(x, y, ...)
```

## Arguments

- x:

  an object of class
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  or
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

- y:

  an object of the same class as `x` (or something which can be coerced
  to that class).

- ...:

  further arguments; unused.

## Value

Returns a new object of the same class as `x` with the items in `y`
added.

## See also

Other preprocessing:
[`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`itemCoding`](http://michael.hahsler.net/arules/reference/itemCoding.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md)

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
[`itemwiseSetOps`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
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

## create a random item as a matrix
randomItem <- sample(c(TRUE, FALSE), size = length(Groceries), replace = TRUE)
randomItem <- as.matrix(randomItem)
colnames(randomItem) <- "random item"
head(randomItem, 3)
#>      random item
#> [1,]        TRUE
#> [2,]       FALSE
#> [3,]       FALSE

## add the random item to Groceries
g2 <- merge(Groceries, randomItem)
nitems(Groceries)
#> [1] 169
nitems(g2)
#> [1] 170
inspect(head(g2, 3))
#>     items                 
#> [1] {citrus fruit,        
#>      semi-finished bread, 
#>      margarine,           
#>      ready soups,         
#>      random item}         
#> [2] {tropical fruit,      
#>      yogurt,              
#>      coffee}              
#> [3] {whole milk}          
```
