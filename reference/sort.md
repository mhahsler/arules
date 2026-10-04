# Sort Associations

Provides the method `sort` to sort elements in class
[associations](http://michael.hahsler.net/arules/reference/associations-class.md)
(e.g., itemsets or rules) according to the value of measures stored in
the association's slot `quality` (e.g., support).

## Usage

``` r
# S4 method for class 'associations'
sort(x, decreasing = TRUE, na.last = NA, by = "support", order = FALSE, ...)
```

## Arguments

- x:

  an object to be sorted.

- decreasing:

  a logical. Should the sort be increasing or decreasing? (default is
  decreasing)

- na.last:

  na.last is not supported for associations. NAs are always put last.

- by:

  a character string specifying the quality measure stored in `x` to be
  used to sort `x`. If a vector of character strings is specified then
  the additional strings are used to sort `x` in case of ties.

- order:

  should a order vector (a permutation like
  [`order()`](https://rdrr.io/r/base/order.html)) be returned instead of
  the sorted associations?

- ...:

  Further arguments are ignored.

## Value

An object of the same class as `x` or a permutation vector.

## Details

`sort` is relatively slow for large sets of associations since it has to
copy and rearrange a large data structure. With `order = TRUE` an
integer vector with the order is returned instead of the reordered
associations.

If only the top `n` associations are needed then
[`head()`](http://michael.hahsler.net/arules/reference/associations-class.md)
using `by` performs this faster than calling `sort()` and then
[`head()`](http://michael.hahsler.net/arules/reference/associations-class.md)
since it does it without copying and rearranging all the data.
[`tail()`](http://michael.hahsler.net/arules/reference/associations-class.md)
works in the same way.

## See also

Other associations functions:
[`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md),
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
[`unique()`](http://michael.hahsler.net/arules/reference/unique.md)

## Author

Michael Hahsler

## Examples

``` r
data("Adult")

## Mine rules with Apriori
rules <- apriori(Adult, parameter = list(supp = 0.6))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.6      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 29305 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[115 item(s), 48842 transaction(s)] done [0.03s].
#> sorting and recoding items ... [6 item(s)] done [0.00s].
#> creating transaction tree ... done [0.01s].
#> checking subsets of size 1 2 3 4 done [0.00s].
#> writing ... [39 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

rules_by_lift <- sort(rules, by = "lift")

inspect(head(rules))
#>     lhs           rhs                            support   confidence coverage
#> [1] {}         => {race=White}                   0.8550428 0.8550428  1.000000
#> [2] {}         => {native-country=United-States} 0.8974243 0.8974243  1.000000
#> [3] {}         => {capital-gain=None}            0.9173867 0.9173867  1.000000
#> [4] {}         => {capital-loss=None}            0.9532779 0.9532779  1.000000
#> [5] {sex=Male} => {capital-gain=None}            0.6050735 0.9051455  0.668482
#> [6] {sex=Male} => {capital-loss=None}            0.6331027 0.9470750  0.668482
#>     lift      count
#> [1] 1.0000000 41762
#> [2] 1.0000000 43832
#> [3] 1.0000000 44807
#> [4] 1.0000000 46560
#> [5] 0.9866565 29553
#> [6] 0.9934931 30922
inspect(head(rules_by_lift))
#>     lhs                               rhs                              support confidence  coverage     lift count
#> [1] {race=White}                   => {native-country=United-States} 0.7881127  0.9217231 0.8550428 1.027076 38493
#> [2] {native-country=United-States} => {race=White}                   0.7881127  0.8781940 0.8974243 1.027076 38493
#> [3] {race=White,                                                                                                  
#>      capital-loss=None}            => {native-country=United-States} 0.7490480  0.9205626 0.8136849 1.025783 36585
#> [4] {race=White,                                                                                                  
#>      capital-gain=None}            => {native-country=United-States} 0.7194628  0.9202807 0.7817862 1.025469 35140
#> [5] {capital-loss=None,                                                                                           
#>      native-country=United-States} => {race=White}                   0.7490480  0.8762454 0.8548380 1.024797 36585
#> [6] {race=White,                                                                                                  
#>      capital-gain=None,                                                                                           
#>      capital-loss=None}            => {native-country=United-States} 0.6803980  0.9189249 0.7404283 1.023958 33232

## A faster/less memory consuming way to get the top 5 rules according to lift
## (see Details section)
inspect(head(rules, n = 5, by = "lift"))
#>     lhs                               rhs                              support confidence  coverage     lift count
#> [1] {race=White}                   => {native-country=United-States} 0.7881127  0.9217231 0.8550428 1.027076 38493
#> [2] {native-country=United-States} => {race=White}                   0.7881127  0.8781940 0.8974243 1.027076 38493
#> [3] {race=White,                                                                                                  
#>      capital-loss=None}            => {native-country=United-States} 0.7490480  0.9205626 0.8136849 1.025783 36585
#> [4] {race=White,                                                                                                  
#>      capital-gain=None}            => {native-country=United-States} 0.7194628  0.9202807 0.7817862 1.025469 35140
#> [5] {capital-loss=None,                                                                                           
#>      native-country=United-States} => {race=White}                   0.7490480  0.8762454 0.8548380 1.024797 36585
```
