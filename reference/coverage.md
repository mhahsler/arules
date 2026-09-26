# Calculate coverage for rules

Provides the generic function and a method to calculate the coverage
(support of the left-hand-side) of
[rules](http://michael.hahsler.net/arules/reference/rules-class.md).

## Usage

``` r
coverage(x, transactions = NULL, reuse = TRUE)

# S4 method for class 'rules'
coverage(x, transactions = NULL, reuse = TRUE)
```

## Arguments

- x:

  the set of
  [rules](http://michael.hahsler.net/arules/reference/rules-class.md).

- transactions:

  the data set used to generate `x`. Only needed if the quality slot of
  `x` does not contain support and confidence.

- reuse:

  reuse support and confidence stored in `x` or recompute from
  transactions?

## Value

A numeric vector of the same length as `x` containing the coverage
values for the sets in `x`.

## Details

Coverage (also called cover or LHS-support) is the support of the
left-hand-side of the rule \\X =\> Y\\, i.e., \\supp(X)\\. It represents
a measure of to how often the rule can be applied.

Coverage can be quickly calculated from the rule's quality measures
(support and confidence) stored in the quality slot. If these values are
not present, then the support of the LHS is counted using the data
supplied in
[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md).

Coverage is also one of the measures available via the function
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md).

## See also

Other interest measures:
[`confint`](http://michael.hahsler.net/arules/reference/confint.md),
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`support()`](http://michael.hahsler.net/arules/reference/support.md)

## Author

Michael Hahsler

## Examples

``` r
data("Income")

## find and some rules (we only use 5 rules here) and calculate coverage
rules <- apriori(Income)[1:5]
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
#> Absolute minimum support count: 687 
#> 
#> set item appearances ...[0 item(s)] done [0.00s].
#> set transactions ...[50 item(s), 6876 transaction(s)] done [0.00s].
#> sorting and recoding items ... [30 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 4 5 6 7 8 done [0.04s].
#> writing ... [8664 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
quality(rules) <- cbind(quality(rules), coverage = coverage(rules))

inspect(rules)
#>     lhs                                 rhs                               support confidence  coverage     lift count  coverage
#> [1] {}                               => {language in home=english}      0.9128854  0.9128854 1.0000000 1.000000  6277 1.0000000
#> [2] {occupation=clerical/service}    => {language in home=english}      0.1127109  0.9292566 0.1212914 1.017933   775 0.1212914
#> [3] {ethnic classification=hispanic} => {education=no college graduate} 0.1096568  0.8636884 0.1269634 1.224731   754 0.1269634
#> [4] {dual incomes=no}                => {marital status=married}        0.1400524  0.9441176 0.1483421 2.447871   963 0.1483421
#> [5] {dual incomes=no}                => {language in home=english}      0.1364165  0.9196078 0.1483421 1.007364   938 0.1483421
```
