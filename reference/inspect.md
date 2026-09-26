# Display Associations and Transactions in Readable Form

Provides the generic function `inspect()` and methods to display
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
and
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
plus additional information formatted for online inspection.

## Usage

``` r
inspect(x, ...)

# S4 method for class 'itemsets'
inspect(x, itemSep = ", ", setStart = "{", setEnd = "}", linebreak = NULL, ...)

# S4 method for class 'rules'
inspect(
  x,
  itemSep = ", ",
  setStart = "{",
  setEnd = "}",
  ruleSep = "=>",
  linebreak = NULL,
  ...
)

# S4 method for class 'transactions'
inspect(x, itemSep = ", ", setStart = "{", setEnd = "}", linebreak = NULL, ...)

# S4 method for class 'itemMatrix'
inspect(x, itemSep = ", ", setStart = "{", setEnd = "}", linebreak = NULL, ...)

# S4 method for class 'tidLists'
inspect(x, ...)
```

## Arguments

- x:

  a set of
  [associations](http://michael.hahsler.net/arules/reference/associations-class.md)
  or
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  or an
  [itemMatrix](http://michael.hahsler.net/arules/reference/itemMatrix-class.md).

- ...:

  additional arguments. can be used to customize the output:

- itemSep:

  item separator

- setStart:

  set start symbol

- setEnd:

  set end symbol

- linebreak:

  print only one element per line in case the output lines get very
  long?

- ruleSep:

  rule separator

## Value

Nothing is returned (see the Details Section).

## Details

`inspect()` prints the results directly. If you need to create a
data.frame with a human readable version, then you can use
[`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md).

## See also

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`associations-class`](http://michael.hahsler.net/arules/reference/associations-class.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
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
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
[`c`](http://michael.hahsler.net/arules/reference/c.md),
[`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md),
[`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md),
[`extract`](http://michael.hahsler.net/arules/reference/extract.md),
[`hierarchy`](http://michael.hahsler.net/arules/reference/hierarchy.md),
[`image`](http://michael.hahsler.net/arules/reference/image.md),
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

Michael Hahsler and Kurt Hornik

## Examples

``` r
data("Adult")
rules <- apriori(Adult)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 4884 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.02s].
#> sorting and recoding items ... [31 item(s)] done [0.00s].
#> creating transaction tree ... done [0.01s].
#> checking subsets of size 1 2 3 4 5 6 7 8 9 done [0.06s].
#> writing ... [6137 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

## display some rules
inspect(rules[1000:1001])
#>     lhs                          rhs                              support confidence  coverage     lift count
#> [1] {education=Some-college,                                                                                 
#>      sex=Male,                                                                                               
#>      capital-loss=None}       => {native-country=United-States} 0.1208181  0.9256471 0.1305229 1.031449  5901
#> [2] {education=Some-college,                                                                                 
#>      sex=Male,                                                                                               
#>      capital-gain=None}       => {capital-loss=None}            0.1199992  0.9474620 0.1266533 0.993899  5861
inspect(rules[1000:1001],
  ruleSep = "~~>", itemSep = " + ", setStart = "", setEnd = "",
  linebreak = FALSE
)
#>     lhs                                                      
#> [1] education=Some-college + sex=Male + capital-loss=None ~~>
#> [2] education=Some-college + sex=Male + capital-gain=None ~~>
#>     rhs                          support   confidence coverage  lift     count
#> [1] native-country=United-States 0.1208181 0.9256471  0.1305229 1.031449 5901 
#> [2] capital-loss=None            0.1199992 0.9474620  0.1266533 0.993899 5861 

## to get rules in readable format, use coercion or DATAFRAME with additional parameters.
as(rules[1000:1001], "data.frame")
#>                                                                                      rules
#> 1000 {education=Some-college,sex=Male,capital-loss=None} => {native-country=United-States}
#> 1001            {education=Some-college,sex=Male,capital-gain=None} => {capital-loss=None}
#>        support confidence  coverage     lift count
#> 1000 0.1208181  0.9256471 0.1305229 1.031449  5901
#> 1001 0.1199992  0.9474620 0.1266533 0.993899  5861
DATAFRAME(rules[1000:1001])
#>                                                      LHS
#> 1000 {education=Some-college,sex=Male,capital-loss=None}
#> 1001 {education=Some-college,sex=Male,capital-gain=None}
#>                                 RHS   support confidence  coverage     lift
#> 1000 {native-country=United-States} 0.1208181  0.9256471 0.1305229 1.031449
#> 1001            {capital-loss=None} 0.1199992  0.9474620 0.1266533 0.993899
#>      count
#> 1000  5901
#> 1001  5861
DATAFRAME(rules[1000:1001], separate = TRUE, setStart = "", setEnd = "")
#>                                                    LHS
#> 1000 education=Some-college,sex=Male,capital-loss=None
#> 1001 education=Some-college,sex=Male,capital-gain=None
#>                               RHS   support confidence  coverage     lift count
#> 1000 native-country=United-States 0.1208181  0.9256471 0.1305229 1.031449  5901
#> 1001            capital-loss=None 0.1199992  0.9474620 0.1266533 0.993899  5861
```
