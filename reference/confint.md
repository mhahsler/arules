# Confidence Intervals for Interest Measures for Association Rules

Defines a method to compute confidence intervals for interest measures
for association
[rules](http://michael.hahsler.net/arules/reference/rules-class.md).

## Usage

``` r
# S3 method for class 'rules'
confint(
  object,
  parm = "oddsRatio",
  level = 0.95,
  measure = NULL,
  side = c("two.sided", "lower", "upper"),
  method = NULL,
  replications = 1000,
  smoothCounts = 0,
  transactions = NULL,
  ...
)
```

## Arguments

- object:

  an object of class
  [rules](http://michael.hahsler.net/arules/reference/rules-class.md).

- parm, measure:

  name of the interest measures (see
  [`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md)).
  `measure` can be used instead of `parm`.

- level:

  the confidence level required.

- side:

  Should a two-sided confidence interval or a one-sided limit be
  returned? Lower returns an interval with only a lower limit and upper
  returns an interval with only an upper limit.

- method:

  method to construct the confidence interval. The available methods
  depends on the measure and the most common method is used by default.

- replications:

  number of replications for method `"bootstrap"`. Ignored for other
  methods.

- smoothCounts:

  pseudo count for addaptive smoothing (Laplace smoothing). Often a
  pseudo counts of .5 is used for smoothing (see Detail Section).

- transactions:

  transactions used to calculate the contingency-table counts. If
  supplied, stored rule-quality values are not reused. An independent
  validation dataset can be supplied to obtain confidence intervals that
  are not affected by mining and selecting the rules on the same
  observations.

- ...:

  Additional parameters are ignored with a warning.

## Value

Returns a matrix with with one row for each rule and the two columns
named `"LL"` and `"UL"` with the interval boundaries. The matrix has the
following additional attributes:

- measure:

  the interest measure.

- level:

  the confidence level

- side:

  the confidence level

- smoothCounts:

  used count smoothing.

- method:

  name of the method to create the interval

- desc:

  description of the used method to calculate the confidence interval.
  The mentioned references can be found below.

## Details

This method creates a contingency table for each rule and then
constructs a confidence interval for the specified measures. Confidence
intervals for all interest measures can be assessed using the
`"bootstrap"` method. However, since bootstrapping has to be applied to
each rule separately, this can be slow. For some popular measures,
faster estimates are available.

## Fast Confidence Interval Estimation

Fast confidence interval approximations are currently available and used
for the measures `"support"`, `"count"`, `"confidence"`, `"lift"`,
`"oddsRatio"`, and `"phi"`.

Methods:

- `"exact"`: Exact binomial proportion confidence interval (Clopper &
  Pearson, 1934).

- `"normal"`: Normal approximation population proportion confidence
  interval (Wilson, 1927).

- `"wilson"`: Wilson score interval (Wilson, 1927).

- `"woolf"`: Woolf method confidence interval for log of the odds ratio
  (Woolf, 1955).

- `"delta"`, `"log_delta"`: Delta and Log delta method (Doob, 1935).

- `"gart"`: Haldane-Anscombe-Gart interval. Delta method with count
  smoothing of .5 (Haldane, 1956).

Available methods by interest measure:

|  |  |  |
|----|----|----|
| Interest measure | Default fast method | Other available fast methods |
| `"count"` | `"wilson"` | `"normal"`, `"exact"` |
| `"support"` | `"wilson"` | `"normal"`, `"exact"` |
| `"confidence"` | `"delta"` | `"log_delta"`, `"wilson"`, `"normal"`, `"exact"` |
| `"lift"` | `"delta"` | `"log_delta"` |
| `"oddsRatio"` | `"woolf"` | `"gart"`, `"exact"` |
| `"phi"` | `"delta"` | None |

The `"bootstrap"` method is also available.

## Count Smoothing

All intervals are calculated using count data. Haldan-Anscombe
correction (Haldan, 1940; Anscombe, 1956) avoids issues with zero counts
by count smoothing (adding .5 to each count). Haldan-Anscombe correction
of `smoothCounts = 0.5` can be used with any interval method.

The Haldane-Anscombe-Gart interval above (method `"gart"`) applies the
delta method with Haldan-Anscombe correction to the odds ratio measure
(Haldane, 1956).

## Using Validation Data

Confidence intervals calculated from the same transactions used to mine
and select rules do not account for the rule-selection process. Their
nominal coverage may therefore be too optimistic, especially when many
candidate rules are examined.

For confirmatory analysis, rules can be mined using training data and an
independent validation transaction set can be supplied using
`transactions`. The contingency-table counts and confidence intervals
are then recalculated from the validation data instead of using the
quality measures stored with the rules. When many rules are evaluated on
the validation data, multiple-comparison adjustments or a further
independent test set may still be appropriate.

## References

Wilson, E. B. (1927). "Probable inference, the law of succession, and
statistical inference". *Journal of the American Statistical
Association,* 22 (158): 209-212.
[doi:10.1080/01621459.1927.10502953](https://doi.org/10.1080/01621459.1927.10502953)

Clopper, C.; Pearson, E. S. (1934). "The use of confidence or fiducial
limits illustrated in the case of the binomial". *Biometrika,* 26 (4):
404-413.
[doi:10.1093/biomet/26.4.404](https://doi.org/10.1093/biomet/26.4.404)

Doob, J. L. (1935). "The Limiting Distributions of Certain Statistics".
*Annals of Mathematical Statistics,* 6: 160-169.
[doi:10.1214/aoms/1177732594](https://doi.org/10.1214/aoms/1177732594)

Fisher, R.A. (1962). "Confidence limits for a cross-product ratio".
*Australian Journal of Statistics,* 4, 41.

Wilson, E.B. (1927, 6). "Probable inference, the law of succession, and
statistical inference". *Journal of the American Statistical
Association,* 22.

Woolf, B. (1955). "On estimating the relation between blood group and
diseases". *Annals of Human Genetics,* 19, 251-253.

Haldane, J.B.S. (1940). "The mean and variance of the moments of
chi-squared when used as a test of homogeneity, when expectations are
small". *Biometrika,* 29, 133-134.

Haldane, J.B.S. (1956, 5). "The estimation and significance of the
logarithm of a ratio of frequencies". *Annals of Human Genetics,* 20.

Anscombe, F.J. (1956). "On estimating binomial response relations".
*Biometrika,* 43, 461-464.

## See also

Other interest measures:
[`coverage()`](http://michael.hahsler.net/arules/reference/coverage.md),
[`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`support()`](http://michael.hahsler.net/arules/reference/support.md)

## Author

Michael Hahsler

## Examples

``` r
data("Income")

# mine some rules with the consequent "language in home=english"
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

# calculate the confidence interval for the rules' odds ratios.
# note that we use Haldane-Anscombe correction (with smoothCounts = .5)
# to avoid issues with 0 counts in the contingency table.
ci <- confint(rules, "oddsRatio", smoothCounts = .5)
ci
#>               LL         UL
#>  [1,]        NaN        NaN
#>  [2,]  1.1749865  1.6437963
#>  [3,]  0.4965793  0.7130648
#>  [4,]  0.8451856  1.1893740
#>  [5,]  0.6943185  0.9837505
#>  [6,]  1.6016605  2.2428021
#>  [7,]  0.4537804  0.6632440
#>  [8,]  1.5103691  2.1158906
#>  [9,] 15.2405124 25.3965063
#> [10,]  2.2036479  3.0915324
#> [11,]  0.4236460  0.6477521
#> [12,]  1.9424309  2.7489168
#> attr(,"se")
#>  [1]        Inf 0.08565252 0.09230506 0.08715113 0.08888978 0.08589063
#>  [7] 0.09682056 0.08600202 0.13027138 0.08636709 0.10832084 0.08859007
#> attr(,"desc")
#> [1] "Woolf method confidence interval for log of the odds ratio (Woolf, 1955). Delta method without count smoothing."
#> attr(,"measure")
#> [1] "oddsRatio"
#> attr(,"level")
#> [1] 0.95
#> attr(,"side")
#> [1] "two.sided"
#> attr(,"smoothCounts")
#> [1] 0.5

# We add the odds ratio (with Haldane-Anscombe correction)
# and the confidence intervals to the quality slot of the rules.
quality(rules) <- cbind(
  quality(rules),
  oddsRatio = interestMeasure(rules, "oddsRatio", smoothCounts = .5),
  oddsRatio = ci
)

rules <- sort(rules, by = "oddsRatio")
inspect(rules)
#>      lhs                                rhs                          support confidence  coverage      lift count  oddsRatio oddsRatio.LL oddsRatio.UL
#> [1]  {ethnic classification=white}   => {language in home=english} 0.6595404  0.9847991 0.6697208 1.0787763  4535 19.5492109   15.2405124   25.3965063
#> [2]  {}                              => {language in home=english} 0.9128854  0.9128854 1.0000000 1.0000000  6277 10.4712260          NaN          NaN
#> [3]  {number in household=1}         => {language in home=english} 0.6495055  0.9388270 0.6918266 1.0284171  4466  2.6094297    2.2036479    3.0915324
#> [4]  {number in household=1,                                                                                                                          
#>       number of children=0}          => {language in home=english} 0.5213787  0.9424290 0.5532286 1.0323629  3585  2.3084164    1.9424309    2.7489168
#> [5]  {number of children=0}          => {language in home=english} 0.5801338  0.9328812 0.6218732 1.0219040  3989  1.8948713    1.6016605    2.2428021
#> [6]  {years in bay area=10+}         => {language in home=english} 0.6013671  0.9300495 0.6465969 1.0188020  4135  1.7877013    1.5103691    2.1158906
#> [7]  {sex=female}                    => {language in home=english} 0.5122164  0.9246521 0.5539558 1.0128896  3522  1.3895135    1.1749865    1.6437963
#> [8]  {type of home=house}            => {language in home=english} 0.5446481  0.9129693 0.5965678 1.0000919  3745  1.0032197    0.8451856    1.1893740
#> [9]  {dual incomes=not married}      => {language in home=english} 0.5426120  0.9069033 0.5983130 0.9934470  3731  0.8272415    0.6943185    0.9837505
#> [10] {age=14-34}                     => {language in home=english} 0.5248691  0.8966460 0.5853694 0.9822109  3609  0.5959378    0.4965793    0.7130648
#> [11] {income=$0-$40,000}             => {language in home=english} 0.5578825  0.8962617 0.6224549 0.9817899  3836  0.5497144    0.4537804    0.6632440
#> [12] {education=no college graduate} => {language in home=english} 0.6343805  0.8995669 0.7052065 0.9854106  4362  0.5255707    0.4236460    0.6477521

# use confidence intervals for lift to find rules with a lift significantly larger then 1.
# We set the confidence level to 95%, create a one-sided interval and check
# if the interval does not cover 1 (i.e., the lower limit is larger than 1).
ci <- confint(rules, "lift", level = 0.95, side = "lower")
ci
#>              LL  UL
#>  [1,] 1.0723784 Inf
#>  [2,] 1.0000000 Inf
#>  [3,] 1.0228963 Inf
#>  [4,] 1.0257459 Inf
#>  [5,] 1.0158722 Inf
#>  [6,] 1.0130021 Inf
#>  [7,] 1.0062100 Inf
#>  [8,] 0.9938294 Inf
#>  [9,] 0.9871905 Inf
#> [10,] 0.9757537 Inf
#> [11,] 0.9758049 Inf
#> [12,] 0.9804520 Inf
#> attr(,"se")
#>  [1] 0.003889650 0.000000000 0.003356408 0.004022847 0.003667023 0.003526105
#>  [7] 0.004060894 0.003807336 0.003803678 0.003925662 0.003638629 0.003014569
#> attr(,"desc")
#> [1] "Delta method confidence interval for lift (Doob, 1935)."
#> attr(,"measure")
#> [1] "lift"
#> attr(,"level")
#> [1] 0.95
#> attr(,"side")
#> [1] "lower"
#> attr(,"smoothCounts")
#> [1] 0

inspect(rules[ci[, "LL"] > 1])
#>     lhs                              rhs                          support confidence  coverage     lift count oddsRatio oddsRatio.LL oddsRatio.UL
#> [1] {ethnic classification=white} => {language in home=english} 0.6595404  0.9847991 0.6697208 1.078776  4535 19.549211    15.240512    25.396506
#> [2] {number in household=1}       => {language in home=english} 0.6495055  0.9388270 0.6918266 1.028417  4466  2.609430     2.203648     3.091532
#> [3] {number in household=1,                                                                                                                      
#>      number of children=0}        => {language in home=english} 0.5213787  0.9424290 0.5532286 1.032363  3585  2.308416     1.942431     2.748917
#> [4] {number of children=0}        => {language in home=english} 0.5801338  0.9328812 0.6218732 1.021904  3989  1.894871     1.601661     2.242802
#> [5] {years in bay area=10+}       => {language in home=english} 0.6013671  0.9300495 0.6465969 1.018802  4135  1.787701     1.510369     2.115891
#> [6] {sex=female}                  => {language in home=english} 0.5122164  0.9246521 0.5539558 1.012890  3522  1.389513     1.174987     1.643796

# For confirmatory analysis, mine rules on training data and calculate
# confidence intervals using independent validation data.
set.seed(1234)
training_ids <- sample(seq_along(Income), floor(.7 * length(Income)))
training <- Income[training_ids]
validation <- Income[-training_ids]

validation_rules <- apriori(training,
  parameter = list(support = .5),
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
#> Absolute minimum support count: 2406 
#> 
#> set item appearances ...[1 item(s)] done [0.00s].
#> set transactions ...[50 item(s), 4813 transaction(s)] done [0.00s].
#> sorting and recoding items ... [11 item(s)] done [0.00s].
#> creating transaction tree ... done [0.00s].
#> checking subsets of size 1 2 3 done [0.00s].
#> writing ... [12 rule(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
validation_ci <- confint(validation_rules,
  "lift",
  transactions = validation,
  side = "lower"
)
inspect(validation_rules[validation_ci[, "LL"] > 1])
#>     lhs                              rhs                          support confidence  coverage     lift count
#> [1] {sex=female}                  => {language in home=english} 0.5063370  0.9234559 0.5483067 1.012897  2437
#> [2] {number of children=0}        => {language in home=english} 0.5827966  0.9291156 0.6272595 1.019105  2805
#> [3] {years in bay area=10+}       => {language in home=english} 0.6004571  0.9292605 0.6461666 1.019264  2890
#> [4] {ethnic classification=white} => {language in home=english} 0.6567629  0.9828980 0.6681903 1.078097  3161
#> [5] {number in household=1}       => {language in home=english} 0.6540619  0.9397015 0.6960316 1.030716  3148
#> [6] {number in household=1,                                                                                  
#>      number of children=0}        => {language in home=english} 0.5254519  0.9411984 0.5582797 1.032358  2529
```
