# Interest measures

No single measure identifies every useful association. Support and
confidence describe prevalence and conditional probability, while
measures such as lift and leverage compare observed co-occurrence with
what would be expected under independence.

``` r

trans <- transactions(list(
  T1 = c("tea", "cookies", "milk"),
  T2 = c("tea", "cookies"),
  T3 = c("coffee", "cookies"),
  T4 = c("tea", "milk"),
  T5 = c("coffee", "milk"),
  T6 = c("tea", "cookies", "milk"),
  T7 = c("coffee", "cookies"),
  T8 = c("tea", "cookies")
))

rules <- apriori(
  trans,
  parameter = list(support = 0.25, confidence = 0.5),
  control = list(verbose = FALSE)
)
```

## Standard interest measures

The quality data frame already contains the measures calculated during
mining.

``` r

head(quality(rules))
#>   support confidence coverage      lift count
#> 1   0.500  0.5000000    1.000 1.0000000     4
#> 2   0.625  0.6250000    1.000 1.0000000     5
#> 3   0.750  0.7500000    1.000 1.0000000     6
#> 4   0.250  0.6666667    0.375 0.8888889     2
#> 5   0.375  0.7500000    0.500 1.2000000     3
#> 6   0.375  0.6000000    0.625 1.2000000     3
```

Different measures answer different questions:

- **Support** is the proportion of transactions containing both sides.
- **Confidence** estimates the conditional probability of the right-hand
  side.
- **Coverage** is the support of the left-hand side.
- **Lift** is confidence divided by the right-hand-side support; values
  above one indicate positive association.

A rare rule can have high lift but little practical impact, while a rule
with high confidence may simply predict a very common consequent. It is
therefore often useful to consider several measures together.

## Calculating additional measures for rules

`arules` implements many commonly used measures. The complete list is in
[A Probabilistic Comparison of Commonly Used Interest Measures for
Association Rules](https://mhahsler.github.io/arules/docs/measures).

[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md)
calculates additional measures. Supply the transactions for measures
that require counts not stored with the rules.

``` r

measures <- interestMeasure(
  rules,
  measure = c("leverage", "phi"),
  transactions = trans
)
head(measures)
#>   leverage        phi
#> 1  0.00000        NaN
#> 2  0.00000        NaN
#> 3  0.00000        NaN
#> 4 -0.03125 -0.1490712
#> 5  0.06250  0.2581989
#> 6  0.06250  0.2581989
```

Here, leverage is the observed joint support minus the support expected
under independence. Phi is the correlation between the left- and
right-hand sides of a rule; it is undefined for some rules.

Add selected measures as new columns in the quality data frame.

``` r

quality(rules) <- cbind(
  quality(rules),
  interestMeasure(
    rules,
    measure = c("leverage", "phi"),
    transactions = trans
  )
)
```

The new measures can now be used to filter and sort rules.

``` r

inspect(head(sort(rules, by = "leverage"), 3))
#>     lhs                rhs    support confidence coverage lift count leverage
#> [1] {cookies, milk} => {tea}  0.250   1.00       0.250    1.6  2     0.09375 
#> [2] {milk}          => {tea}  0.375   0.75       0.500    1.2  3     0.06250 
#> [3] {tea}           => {milk} 0.375   0.60       0.625    1.2  3     0.06250 
#>     phi      
#> [1] 0.4472136
#> [2] 0.2581989
#> [3] 0.2581989
```
