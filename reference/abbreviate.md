# Abbreviate item labels in transactions, itemMatrix and associations

Provides the generic function and the methods to abbreviate long item
labels in transactions, associations (rules and itemsets) and
transaction ID lists. Note that `abbreviate()` is not a generic and this
arules defines a generic with the
[`base::abbreviate()`](https://rdrr.io/r/base/abbreviate.html) as the
default.

## Usage

``` r
abbreviate(names.arg, ...)

# S4 method for class 'itemMatrix'
abbreviate(names.arg, minlength = 4, ..., method = "both.sides")

# S4 method for class 'transactions'
abbreviate(names.arg, minlength = 4, ..., method = "both.sides")

# S4 method for class 'rules'
abbreviate(names.arg, minlength = 4, ..., method = "both.sides")

# S4 method for class 'itemsets'
abbreviate(names.arg, minlength = 4, ..., method = "both.sides")

# S4 method for class 'tidLists'
abbreviate(names.arg, minlength = 4, ..., method = "both.sides")
```

## Arguments

- names.arg:

  an object of class
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md),
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md),
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
  [rules](http://michael.hahsler.net/arules/reference/rules-class.md) or
  [tidLists](http://michael.hahsler.net/arules/reference/tidLists-class.md).

- ...:

  further arguments passed on to the default abbreviation function.

- minlength:

  number of characters allowed in abbreviation

- method:

  apply to level and value (both.sides)

## See also

[`base::abbreviate()`](https://rdrr.io/r/base/abbreviate.html)

Other associations functions:
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`itemsets-class`](http://michael.hahsler.net/arules/reference/itemsets-class.md),
[`match()`](http://michael.hahsler.net/arules/reference/match.md),
[`rules-class`](http://michael.hahsler.net/arules/reference/rules-class.md),
[`sample()`](http://michael.hahsler.net/arules/reference/sample.md),
[`sets`](http://michael.hahsler.net/arules/reference/sets.md),
[`size()`](http://michael.hahsler.net/arules/reference/size.md),
[`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

Other itemMatrix and transactions functions:
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

Sudheer Chelluboina and Michael Hahsler based on code by Martin
Vodenicharov.

## Examples

``` r

data(Adult)
inspect(head(Adult, 1))
#>     items                           transactionID
#> [1] {age=Middle-aged,                            
#>      workclass=State-gov,                        
#>      education=Bachelors,                        
#>      marital-status=Never-married,               
#>      occupation=Adm-clerical,                    
#>      relationship=Not-in-family,                 
#>      race=White,                                 
#>      sex=Male,                                   
#>      capital-gain=Low,                           
#>      capital-loss=None,                          
#>      hours-per-week=Full-time,                   
#>      native-country=United-States,               
#>      income=small}                              1

Adult_abbr <- abbreviate(Adult, 15)
inspect(head(Adult_abbr, 1))
#>     items             
#> [1] {age=Middle-aged, 
#>      workclss=Stt-gv, 
#>      educatin=Bchlrs, 
#>      mrtl-stts=Nvr-m, 
#>      occptn=Adm-clrc, 
#>      rltnshp=Nt-n-fm, 
#>      race=White,      
#>      sex=Male,        
#>      capital-gain=Lw, 
#>      capital-loss=Nn, 
#>      hrs-pr-wk=Fll-t, 
#>      ntv-cntry=Unt-S, 
#>      income=small}    
```
