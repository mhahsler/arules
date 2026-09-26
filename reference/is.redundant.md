# Find Redundant Rules

Provides the generic function `is.redundant()` and the method to find
redundant rules based on any interest measure.

## Usage

``` r
is.redundant(x, ...)

# S4 method for class 'rules'
is.redundant(
  x,
  measure = "confidence",
  confint = FALSE,
  level = 0.95,
  smoothCounts = 1,
  ...
)
```

## Arguments

- x:

  a set of rules.

- ...:

  additional arguments are passed on to
  [`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md),
  or, for `confint = TRUE` to
  [`confint()`](http://michael.hahsler.net/arules/reference/confint.md).

- measure:

  measure used to check for redundancy.

- confint:

  should confidence intervals be used to the redundancy check?

- level:

  confidence level for the confidence interval. Only used when
  `confint = TRUE`.

- smoothCounts:

  adds a "pseudo count" to each count in the used contingency table.
  This implements addaptive smoothing (Laplace smoothing) for counts and
  avoids zero counts.

## Value

returns a logical vector indicating which rules are redundant.

## Details

**Simple improvement-based redundancy:** (`confint = FALSE`) A rule can
be defined as redundant if a more general rules with the same or a
higher confidence exists. That is, a more specific rule is redundant if
it is only equally or even less predictive than a more general rule. A
rule is more general if it has the same RHS but one or more items
removed from the LHS. Formally, a rule \\X \Rightarrow Y\\ is redundant
if

\$\$\exists X' \subset X \quad conf(X' \Rightarrow Y) \ge conf(X
\Rightarrow Y).\$\$

This is equivalent to a negative or zero *improvement* as defined by
Bayardo et al. (2000).

The idea of improvement can be extended other measures besides
confidence. Any other measure available for function
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md)
(e.g., lift or the odds ratio) can be specified in `measure`.

**Confidence interval-based redundancy:** (`confint = TRUE`) Li et al
(2014) propose to use the confidence interval (CI) of the odds ratio
(OR) of rules to define redundancy. A more specific rule is redundant if
it does not provide a significantly higher OR than any more general
rule. Using confidence intervals as error bounds, a more specific rule
is defined as redundant if its OR CI overlaps with the CI of any more
general rule. This type of redundancy detection removes more rules than
improvement since it takes differences in counts due to randomness in
the dataset into account.

The odds ratio and the CI are based on counts which can be zero and
which leads to numerical problems. In addition to the method described
by Li et al (2014), we use additive smoothing (Laplace smoothing) to
alleviate this problem. The default setting adds 1 to each count (see
[`confint()`](http://michael.hahsler.net/arules/reference/confint.md)).
A different pseudocount (smoothing parameter) can be defined using the
additional parameter `smoothCounts`. Smoothing can be disabled using
`smoothCounts = 0`.

**Warning:** This approach of redundancy checking is flawed since rules
with non-overlapping CIs are non-redundant (same result as for a
2-sample t-test), but overlapping CIs do not automatically mean that
there is no significant difference between the two measures which leads
to a higher type II error. At the same time, multiple comparisons are
performed leading to an increased type I error. If we are more worried
about missing important rules, then the type II error is more
concerning.

Confidence interval-based redundancy checks can also be used for other
measures with a confidence interval like confidence (see
[`confint()`](http://michael.hahsler.net/arules/reference/confint.md)).

## References

Bayardo, R. , R. Agrawal, and D. Gunopulos (2000). Constraint-based rule
mining in large, dense databases. *Data Mining and Knowledge Discovery,*
4(2/3):217–240.

Li, J., Jixue Liu, Hannu Toivonen, Kenji Satou, Youqiang Sun, and Bingyu
Sun (2014). Discovering statistically non-redundant subgroups.
Knowledge-Based Systems. 67 (September, 2014), 315–327.
[doi:10.1016/j.knosys.2014.04.030](https://doi.org/10.1016/j.knosys.2014.04.030)

## See also

Other postprocessing:
[`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
[`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md),
[`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md),
[`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)

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

Other interest measures:
[`confint`](http://michael.hahsler.net/arules/reference/confint.md),
[`coverage()`](http://michael.hahsler.net/arules/reference/coverage.md),
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`support()`](http://michael.hahsler.net/arules/reference/support.md)

## Author

Michael Hahsler and Christian Buchta

## Examples

``` r

data("Income")

## mine some rules with the consequent "language in home=english"
rules <- apriori(Income,
  parameter = list(support = 0.5),
  appearance = list(rhs = "language in home=english")
)
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>         0.8    0.1    1 none FALSE            TRUE       5     0.5      1
#>  maxlen target  ext
#>      10  rules TRUE
#> 
#> Algorithmic control:
#>  filter tree heap memopt load sort verbose
#>     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
#> 
#> Absolute minimum support count: 3438 
#> 
#> set item appearances ...[1 item(s)] done [0.00s].
#> set transactions ...[50 item(s), 6876 transaction(s)] done [0.00s].
#> sorting and recoding items ... [11 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 done [0.00s].
#> writing ... [12 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].

## for better comparison we add Bayado's improvement and sort by improvement
quality(rules)$improvement <- interestMeasure(rules, measure = "improvement")
rules <- sort(rules, by = "improvement")
inspect(rules)
#>      lhs                                rhs                          support confidence  coverage      lift count   improvement
#> [1]  {}                              => {language in home=english} 0.9128854  0.9128854 1.0000000 1.0000000  6277  9.128854e-01
#> [2]  {ethnic classification=white}   => {language in home=english} 0.6595404  0.9847991 0.6697208 1.0787763  4535  7.191373e-02
#> [3]  {number in household=1}         => {language in home=english} 0.6495055  0.9388270 0.6918266 1.0284171  4466  2.594159e-02
#> [4]  {number of children=0}          => {language in home=english} 0.5801338  0.9328812 0.6218732 1.0219040  3989  1.999580e-02
#> [5]  {years in bay area=10+}         => {language in home=english} 0.6013671  0.9300495 0.6465969 1.0188020  4135  1.716408e-02
#> [6]  {sex=female}                    => {language in home=english} 0.5122164  0.9246521 0.5539558 1.0128896  3522  1.176674e-02
#> [7]  {number in household=1,                                                                                                   
#>       number of children=0}          => {language in home=english} 0.5213787  0.9424290 0.5532286 1.0323629  3585  3.602030e-03
#> [8]  {type of home=house}            => {language in home=english} 0.5446481  0.9129693 0.5965678 1.0000919  3745  8.388479e-05
#> [9]  {dual incomes=not married}      => {language in home=english} 0.5426120  0.9069033 0.5983130 0.9934470  3731 -5.982141e-03
#> [10] {education=no college graduate} => {language in home=english} 0.6343805  0.8995669 0.7052065 0.9854106  4362 -1.331848e-02
#> [11] {age=14-34}                     => {language in home=english} 0.5248691  0.8966460 0.5853694 0.9822109  3609 -1.623944e-02
#> [12] {income=$0-$40,000}             => {language in home=english} 0.5578825  0.8962617 0.6224549 0.9817899  3836 -1.662372e-02
is.redundant(rules)
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE

## find non-redundant rules using improvement of confidence
## Note: a few rules have a very small improvement over the rule {} => {language in home=english}
rules_non_redundant <- rules[!is.redundant(rules)]
inspect(rules_non_redundant)
#>     lhs                              rhs                          support confidence  coverage     lift count  improvement
#> [1] {}                            => {language in home=english} 0.9128854  0.9128854 1.0000000 1.000000  6277 9.128854e-01
#> [2] {ethnic classification=white} => {language in home=english} 0.6595404  0.9847991 0.6697208 1.078776  4535 7.191373e-02
#> [3] {number in household=1}       => {language in home=english} 0.6495055  0.9388270 0.6918266 1.028417  4466 2.594159e-02
#> [4] {number of children=0}        => {language in home=english} 0.5801338  0.9328812 0.6218732 1.021904  3989 1.999580e-02
#> [5] {years in bay area=10+}       => {language in home=english} 0.6013671  0.9300495 0.6465969 1.018802  4135 1.716408e-02
#> [6] {sex=female}                  => {language in home=english} 0.5122164  0.9246521 0.5539558 1.012890  3522 1.176674e-02
#> [7] {number in household=1,                                                                                               
#>      number of children=0}        => {language in home=english} 0.5213787  0.9424290 0.5532286 1.032363  3585 3.602030e-03
#> [8] {type of home=house}          => {language in home=english} 0.5446481  0.9129693 0.5965678 1.000092  3745 8.388479e-05

## use non-overlapping confidence intervals for the confidence measure instead
## Note: fewer rules have a significantly higher confidence
inspect(rules[!is.redundant(rules,
  measure = "confidence",
  confint = TRUE, level = 0.95
)])
#>     lhs                              rhs                          support confidence  coverage     lift count improvement
#> [1] {}                            => {language in home=english} 0.9128854  0.9128854 1.0000000 1.000000  6277  0.91288540
#> [2] {ethnic classification=white} => {language in home=english} 0.6595404  0.9847991 0.6697208 1.078776  4535  0.07191373
#> [3] {number in household=1}       => {language in home=english} 0.6495055  0.9388270 0.6918266 1.028417  4466  0.02594159
#> [4] {number of children=0}        => {language in home=english} 0.5801338  0.9328812 0.6218732 1.021904  3989  0.01999580
#> [5] {years in bay area=10+}       => {language in home=english} 0.6013671  0.9300495 0.6465969 1.018802  4135  0.01716408

## find non-redundant rules using improvement of the odds ratio.
quality(rules)$oddsRatio <- interestMeasure(rules, measure = "oddsRatio", smoothCounts = .5)
inspect(rules[!is.redundant(rules, measure = "oddsRatio")])
#>     lhs                              rhs                          support confidence  coverage     lift count improvement oddsRatio
#> [1] {}                            => {language in home=english} 0.9128854  0.9128854 1.0000000 1.000000  6277  0.91288540  10.47123
#> [2] {ethnic classification=white} => {language in home=english} 0.6595404  0.9847991 0.6697208 1.078776  4535  0.07191373  19.54921

## use the confidence interval for the odds ratio.
## We see that no rule has a significantly better odds ratio than the most general rule.
inspect(rules[!is.redundant(rules,
  measure = "oddsRatio",
  confint = TRUE, level = 0.95
)])
#> Warning: Subsetting with NAs. NAs are omitted!

##  use the confidence interval for lift
inspect(rules[!is.redundant(rules,
  measure = "lift",
  confint = TRUE, level = 0.95
)])
#>     lhs                              rhs                          support confidence  coverage     lift count improvement oddsRatio
#> [1] {}                            => {language in home=english} 0.9128854  0.9128854 1.0000000 1.000000  6277  0.91288540 10.471226
#> [2] {ethnic classification=white} => {language in home=english} 0.6595404  0.9847991 0.6697208 1.078776  4535  0.07191373 19.549211
#> [3] {number in household=1}       => {language in home=english} 0.6495055  0.9388270 0.6918266 1.028417  4466  0.02594159  2.609430
#> [4] {number of children=0}        => {language in home=english} 0.5801338  0.9328812 0.6218732 1.021904  3989  0.01999580  1.894871
#> [5] {years in bay area=10+}       => {language in home=english} 0.6013671  0.9300495 0.6465969 1.018802  4135  0.01716408  1.787701
#> [6] {sex=female}                  => {language in home=english} 0.5122164  0.9246521 0.5539558 1.012890  3522  0.01176674  1.389513
```
