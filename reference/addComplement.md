# Add Complement-items to Transactions

Provides the generic function `addComplement()` and a method for
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
to add complement items. That is, it adds an artificial item to each
transaction which does not contain the original item. Such items are
also called negative items (Antonie et al, 2014).

## Usage

``` r
addComplement(x, labels, complementLabels = NULL)

# S4 method for class 'transactions'
addComplement(x, labels, complementLabels = NULL)
```

## Arguments

- x:

  an object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

- labels:

  character strings; item labels for which complements should be
  created.

- complementLabels:

  character strings; labels for the artificial complement-items. If
  omitted then the original label is prepended by "!" to form the
  complement-item label.

## Value

Returns an object of class
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
with complement items added.

## References

Antonie L., Li J., Zaiane O. (2014) Negative Association Rules. In:
Aggarwal C., Han J. (eds) *Frequent Pattern Mining,* Springer
International Publishing, pp. 135-145.
[doi:10.1007/978-3-319-07821-2_6](https://doi.org/10.1007/978-3-319-07821-2_6)

## Author

Michael Hahsler

## Examples

``` r

data("Groceries")

## add a complement-items for "whole milk" and "other vegetables"
g2 <- addComplement(Groceries, c("whole milk", "other vegetables"))
g2
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  171 items (columns)
tail(itemInfo(g2))
#>                     labels level2   level1              variables levels
#> 166 flower soil/fertilizer garden non-food flower soil/fertilizer   <NA>
#> 167         flower (seeds) garden non-food         flower (seeds)   <NA>
#> 168          shopping bags   bags non-food          shopping bags   <NA>
#> 169                   bags   bags non-food                   bags   <NA>
#> 170            !whole milk   <NA>     <NA>             whole milk  FALSE
#> 171      !other vegetables   <NA>     <NA>       other vegetables  FALSE
inspect(head(g2, 3))
#>     items                 
#> [1] {citrus fruit,        
#>      semi-finished bread, 
#>      margarine,           
#>      ready soups,         
#>      !whole milk,         
#>      !other vegetables}   
#> [2] {tropical fruit,      
#>      yogurt,              
#>      coffee,              
#>      !whole milk,         
#>      !other vegetables}   
#> [3] {whole milk,          
#>      !other vegetables}   

## use a custom label for the complement-item
g3 <- addComplement(g2, "coffee", complementLabels = "NO coffee")
inspect(head(g2, 3))
#>     items                 
#> [1] {citrus fruit,        
#>      semi-finished bread, 
#>      margarine,           
#>      ready soups,         
#>      !whole milk,         
#>      !other vegetables}   
#> [2] {tropical fruit,      
#>      yogurt,              
#>      coffee,              
#>      !whole milk,         
#>      !other vegetables}   
#> [3] {whole milk,          
#>      !other vegetables}   

## add complements for all items (this is excessive for this dataset)
g4 <- addComplement(Groceries, itemLabels(Groceries))
g4
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  338 items (columns)

## add complements for all items with a minimum support of 0.1
g5 <- addComplement(Groceries, names(which(itemFrequency(Groceries) >= 0.1)))
g5
#> transactions in sparse format with
#>  9835 transactions (rows) and
#>  177 items (columns)
```
