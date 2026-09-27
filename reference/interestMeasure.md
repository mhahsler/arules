# Calculate Additional Interest Measures

Provides the generic function `interestMeasure()` and the methods to
calculate various additional interest measures for existing sets of
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or [rules](http://michael.hahsler.net/arules/reference/rules-class.md).

## Usage

``` r
interestMeasure(x, measure, transactions = NULL, reuse = TRUE, ...)

# S4 method for class 'itemsets'
interestMeasure(x, measure, transactions = NULL, reuse = TRUE, ...)

# S4 method for class 'rules'
interestMeasure(x, measure, transactions = NULL, reuse = TRUE, ...)
```

## Arguments

- x:

  a set of
  [itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  or
  [rules](http://michael.hahsler.net/arules/reference/rules-class.md).

- measure:

  name or vector of names of the desired interest measures (see the
  Details section for available measures). If measure is missing then
  all available measures are calculated.

- transactions:

  the
  [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
  used to mine the associations or a set of different transactions to
  calculate interest measures from (Note: you need to set
  `reuse = FALSE` in the later case).

- reuse:

  logical indicating if information in the quality slot should be reuse
  for calculating the measures. This speeds up the process significantly
  since only very little (or no) transaction counting is necessary if
  support, confidence and lift are already available. Use
  `reuse = FALSE` to force counting (might be very slow but is necessary
  if you use a different set of transactions than was used for mining).

- ...:

  further arguments for the measure calculation. Many measures are based
  on contingency table counts and zero counts can produce `NaN` values
  (division by zero). This issue can be resolved by using the additional
  parameter `smoothCounts` which performs additive smoothing by adds a
  "pseudo count" of `smoothCounts` to each cell in the contingency
  table. Use `smoothCounts = 1` or larger values for Laplace smoothing.
  Use `smoothCounts = .5` for Haldane-Anscombe correction (Haldan, 1940;
  Anscombe, 1956) which is often used for chi-squared, phi correlation
  and related measures.

## Value

If only one measure is used, the function returns a numeric vector
containing the values of the interest measure for each association in
the set of associations `x`.

If more than one measures are specified, the result is a data.frame
containing the different measures for each association as columns.

`NA` is returned for rules/itemsets for which a certain measure is not
defined.

## Details

A searchable list of definitions, equations and references for all
available interest measures can be found at
<https://mhahsler.github.io/arules/docs/measures>. The descriptions are
also linked in the list below.

The following measures are implemented for **itemsets**:

- "support":
  [Support.](https://mhahsler.github.io/arules/docs/measures#support)

- "count": [Support
  Count.](https://mhahsler.github.io/arules/docs/measures#count)

- "allConfidence":
  [All-Confidence.](https://mhahsler.github.io/arules/docs/measures#allconfidence)

- "crossSupportRatio": [Cross-Support
  Ratio.](https://mhahsler.github.io/arules/docs/measures#crosssupportratio)

- "lift": [Lift.](https://mhahsler.github.io/arules/docs/measures#lift)

The following measures are implemented for **rules**:

- "support":
  [Support.](https://mhahsler.github.io/arules/docs/measures#support)

- "confidence":
  [Confidence.](https://mhahsler.github.io/arules/docs/measures#confidence)

- "lift": [Lift.](https://mhahsler.github.io/arules/docs/measures#lift)

- "count": [Support
  Count.](https://mhahsler.github.io/arules/docs/measures#count)

- "addedValue": [Added
  Value.](https://mhahsler.github.io/arules/docs/measures#addedvalue)

- "boost": [Confidence
  Boost.](https://mhahsler.github.io/arules/docs/measures#boost)

- "causalConfidence": [Causal
  Confidence.](https://mhahsler.github.io/arules/docs/measures#causalconfidence)

- "causalSupport": [Causal
  Support.](https://mhahsler.github.io/arules/docs/measures#causalsupport)

- "accuracy":
  [Accuracy.](https://mhahsler.github.io/arules/docs/measures#accuracy)

- "balancedAccuracy": [Balanced
  Accuracy.](https://mhahsler.github.io/arules/docs/measures#balancedaccuracy)

- "centeredConfidence": [Centered
  Confidence.](https://mhahsler.github.io/arules/docs/measures#centeredconfidence)

- "certainty": [Certainty
  Factor.](https://mhahsler.github.io/arules/docs/measures#certainty)

- "chiSquared":
  [Chi-Squared.](https://mhahsler.github.io/arules/docs/measures#chisquared)
  Additional parameters are: `significance = TRUE` returns the p-value
  of the test for independence instead of the chi-squared statistic. For
  p-values, substitution effects (the occurrence of one item makes the
  occurrence of another item less likely) can be tested using the
  parameter `complements = FALSE`. Note: Correction for multiple
  comparisons can be done using
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

- "collectiveStrength": [Collective
  Strength.](https://mhahsler.github.io/arules/docs/measures#collectivestrength)

- "confirmedConfidence": [Descriptive Confirmed
  Confidence.](https://mhahsler.github.io/arules/docs/measures#confirmedconfidence)

- "conviction":
  [Conviction.](https://mhahsler.github.io/arules/docs/measures#conviction)

- "cosine":
  [Cosine.](https://mhahsler.github.io/arules/docs/measures#cosine)

- "counterexample": [Example and Counter-Example
  Rate.](https://mhahsler.github.io/arules/docs/measures#counterexample)

- "coverage":
  [Coverage.](https://mhahsler.github.io/arules/docs/measures#coverage)

- "doc": [Difference of
  Confidence.](https://mhahsler.github.io/arules/docs/measures#doc)

- "fishersExactTest": [Fisher's Exact
  Test.](https://mhahsler.github.io/arules/docs/measures#fishersexacttest)
  By default complementary effects are mined, substitutes can be found
  by using the parameter `complements = FALSE`. Note that Fisher's exact
  test is equal to hyper-confidence with `significance = TRUE`.
  Correction for multiple comparisons can be done using
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

- "gini": [Gini
  Index.](https://mhahsler.github.io/arules/docs/measures#gini)

- "hyperConfidence":
  [Hyper-Confidence.](https://mhahsler.github.io/arules/docs/measures#hyperconfidence)
  Reports the confidence level by default and the significance level if
  `significance = TRUE` is used. By default complementary effects are
  mined, substitutes (too low co-occurrence counts) can be found by
  using the parameter `complements = FALSE`.

- "hyperLift":
  [Hyper-Lift.](https://mhahsler.github.io/arules/docs/measures#hyperlift)
  The used quantile can be changed using parameter `level` (default:
  `level = 0.99`).

- "imbalance": [Imbalance
  Ratio.](https://mhahsler.github.io/arules/docs/measures#imbalance)

- "implicationIndex": [Implication
  Index.](https://mhahsler.github.io/arules/docs/measures#implicationindex)

- "importance":
  [Importance.](https://mhahsler.github.io/arules/docs/measures#importance)

- "improvement":
  [Improvement.](https://mhahsler.github.io/arules/docs/measures#improvement)
  The additional parameter `improvementMeasure` (default:
  `'confidence'`) can be used to specify the measure used for the
  improvement calculation. See [Generalized
  improvement](https://mhahsler.github.io/arules/docs/measures#generalizedImprovement).

- "jaccard": [Jaccard
  Coefficient.](https://mhahsler.github.io/arules/docs/measures#jaccard)

- "jMeasure":
  [J-Measure.](https://mhahsler.github.io/arules/docs/measures#jmeasure)

- "kappa":
  [Kappa.](https://mhahsler.github.io/arules/docs/measures#kappa)

- "kulczynski":
  [Kulczynski.](https://mhahsler.github.io/arules/docs/measures#kulczynski)

- "lambda":
  [Lambda.](https://mhahsler.github.io/arules/docs/measures#lambda)

- "laplace": [Laplace Corrected
  Confidence.](https://mhahsler.github.io/arules/docs/measures#laplace)
  Parameter `k` can be used to specify the number of classes (default is
  2).

- "leastContradiction": [Least
  Contradiction.](https://mhahsler.github.io/arules/docs/measures#leastcontradiction)

- "lerman": [Lerman
  Similarity.](https://mhahsler.github.io/arules/docs/measures#lerman)

- "leverage":
  [Leverage.](https://mhahsler.github.io/arules/docs/measures#leverage)

- "LIC": [Lift
  Increase.](https://mhahsler.github.io/arules/docs/measures#lic) The
  additional parameter `improvementMeasure` (default: `'lift'`) can be
  used to specify the measure used for the increase calculation. See
  [Generalized increase
  ratio](https://mhahsler.github.io/arules/docs/measures#ginc).

- "maxconfidence":
  [Max-Confidence.](https://mhahsler.github.io/arules/docs/measures#maxconfidence)

- "mutualInformation": [Mutual
  Information.](https://mhahsler.github.io/arules/docs/measures#mutualinformation)

- "netconf":
  [Netconf.](https://mhahsler.github.io/arules/docs/measures#netconf)

- "oddsRatio": [Odds
  Ratio.](https://mhahsler.github.io/arules/docs/measures#oddsratio)

- "phi": [Phi Correlation
  Coefficient.](https://mhahsler.github.io/arules/docs/measures#phi)

- "ralambondrainy":
  [Ralambondrainy.](https://mhahsler.github.io/arules/docs/measures#ralambondrainy)

- "relativeRisk": [Relative
  Risk.](https://mhahsler.github.io/arules/docs/measures#relativerisk)

- "rhsSupport": [Right-Hand-Side
  Support.](https://mhahsler.github.io/arules/docs/measures#rhssupport)

- "RLD": [Relative Linkage
  Disequilibrium.](https://mhahsler.github.io/arules/docs/measures#rld)

- "rulePowerFactor": [Rule Power
  Factor.](https://mhahsler.github.io/arules/docs/measures#rulepowerfactor)

- "precision":
  [Precision.](https://mhahsler.github.io/arules/docs/measures#precision)

- "recall":
  [Recall.](https://mhahsler.github.io/arules/docs/measures#recall)

- "fScore":
  [F-score.](https://mhahsler.github.io/arules/docs/measures#fscore)

- "sebag":
  [Sebag-Schoenauer.](https://mhahsler.github.io/arules/docs/measures#sebag)

- "stdLift": [Standardized
  Lift.](https://mhahsler.github.io/arules/docs/measures#stdlift)

- "table": [Contingency
  Table.](https://mhahsler.github.io/arules/docs/measures#table) Returns
  the four counts for the contingency table. The entries are labeled
  `n11`, `n01`, `n10`, and `n00` (the first subscript is for X and the
  second is for Y; 1 indicated presence and 0 indicates absence). If
  several measures are specified, then the counts have the prefix
  `table.`

- "varyingLiaison": [Varying Rates
  Liaison.](https://mhahsler.github.io/arules/docs/measures#varyingliaison)

- "zhang": [Zhang's
  Measure.](https://mhahsler.github.io/arules/docs/measures#zhang)

- "yuleQ": [Yule's
  Q.](https://mhahsler.github.io/arules/docs/measures#yuleq)

- "yuleY": [Yule's
  Y.](https://mhahsler.github.io/arules/docs/measures#yuley)

## References

Hahsler, Michael (2015). A Probabilistic Comparison of Commonly Used
Interest Measures for Association Rules, 2015, URL:
<https://mhahsler.github.io/arules/docs/measures>.

Haldane, J.B.S. (1940). "The mean and variance of the moments of
chi-squared when used as a test of homogeneity, when expectations are
small". *Biometrika,* 29, 133-134.

Anscombe, F.J. (1956). "On estimating binomial response relations".
*Biometrika,* 43, 461-464.

## See also

[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md),
[rules](http://michael.hahsler.net/arules/reference/rules-class.md)

Other interest measures:
[`confint`](http://michael.hahsler.net/arules/reference/confint.md),
[`coverage()`](http://michael.hahsler.net/arules/reference/coverage.md),
[`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
[`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md),
[`support()`](http://michael.hahsler.net/arules/reference/support.md)

## Author

Michael Hahsler

## Examples

``` r

data("Income")
rules <- apriori(Income)
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

## calculate a single measure and add it to the quality slot
quality(rules) <- cbind(quality(rules),
  hyperConfidence = interestMeasure(rules,
    measure = "hyperConfidence",
    transactions = Income
  )
)

inspect(head(rules, by = "hyperConfidence"))
#>     lhs                                 rhs                               support confidence  coverage     lift count hyperConfidence
#> [1] {ethnic classification=hispanic} => {education=no college graduate} 0.1096568  0.8636884 0.1269634 1.224731   754               1
#> [2] {dual incomes=no}                => {marital status=married}        0.1400524  0.9441176 0.1483421 2.447871   963               1
#> [3] {occupation=student}             => {marital status=single}         0.1449971  0.8838652 0.1640489 2.160490   997               1
#> [4] {occupation=student}             => {age=14-34}                     0.1592496  0.9707447 0.1640489 1.658345  1095               1
#> [5] {occupation=student}             => {dual incomes=not married}      0.1535777  0.9361702 0.1640489 1.564683  1056               1
#> [6] {occupation=student}             => {income=$0-$40,000}             0.1381617  0.8421986 0.1640489 1.353027   950               1

## calculate several measures
m <- interestMeasure(rules, c("confidence", "oddsRatio", "leverage"),
  transactions = Income
)
inspect(head(rules))
#>     lhs                                 rhs                               support confidence  coverage     lift count hyperConfidence
#> [1] {}                               => {language in home=english}      0.9128854  0.9128854 1.0000000 1.000000  6277       0.0000000
#> [2] {occupation=clerical/service}    => {language in home=english}      0.1127109  0.9292566 0.1212914 1.017933   775       0.9601859
#> [3] {ethnic classification=hispanic} => {education=no college graduate} 0.1096568  0.8636884 0.1269634 1.224731   754       1.0000000
#> [4] {dual incomes=no}                => {marital status=married}        0.1400524  0.9441176 0.1483421 2.447871   963       1.0000000
#> [5] {dual incomes=no}                => {language in home=english}      0.1364165  0.9196078 0.1483421 1.007364   938       0.7763615
#> [6] {occupation=student}             => {marital status=single}         0.1449971  0.8838652 0.1640489 2.160490   997       1.0000000
head(m)
#>   confidence oddsRatio     leverage
#> 1  0.9128854       NaN 0.0000000000
#> 2  0.9292566  1.289208 0.0019856861
#> 3  0.8636884  2.952221 0.0201213950
#> 4  0.9441176 41.681686 0.0828384029
#> 5  0.9196078  1.107694 0.0009972213
#> 6  0.8838652 16.478646 0.0778840228

## calculate all available measures for the first 5 rules and show them as a
## table with the measures as rows
t(interestMeasure(head(rules, 5), transactions = Income))
#>                             [,1]          [,2]          [,3]          [,4]
#> support                0.9128854  1.127109e-01  1.096568e-01  1.400524e-01
#> confidence             0.9128854  9.292566e-01  8.636884e-01  9.441176e-01
#> lift                   1.0000000  1.017933e+00  1.224731e+00  2.447871e+00
#> count               6277.0000000  7.750000e+02  7.540000e+02  9.630000e+02
#> addedValue             0.0000000  1.637120e-02  1.584819e-01  5.584283e-01
#> boost                        Inf  1.017933e+00           Inf           Inf
#> causalConfidence       0.4564427  9.153795e-01  9.024905e-01  9.653117e-01
#> causalSupport          0.9128854  1.912449e-01  3.871437e-01  7.460733e-01
#> accuracy               0.9128854  1.912449e-01  3.871437e-01  7.460733e-01
#> balancedAccuracy       0.5000000  5.124846e-01  5.483943e-01  6.748139e-01
#> centeredConfidence     0.0000000  1.637120e-02  1.584819e-01  5.584283e-01
#> certainty              0.0000000  1.879271e-01  5.376032e-01  9.090324e-01
#> chiSquared                   NaN  3.198710e+00  1.208111e+02  1.576319e+03
#> collectiveStrength     1.0000000  1.026221e+00  1.189288e+00  2.124161e+00
#> confirmedConfidence    0.8257708  8.585132e-01  7.273769e-01  8.882353e-01
#> conviction             1.0000000  1.231417e+00  2.162645e+00  1.099293e+01
#> cosine                 0.9554504  3.387214e-01  3.664698e-01  5.855169e-01
#> counterexample         0.9045722  9.238710e-01  8.421751e-01  9.408100e-01
#> coverage               1.0000000  1.212914e-01  1.269634e-01  1.483421e-01
#> doc                          NaN  1.863097e-02  1.815295e-01  6.556955e-01
#> fishersExactTest       1.0000000  3.981412e-02  9.791397e-32  0.000000e+00
#> gini                         NaN  7.399053e-05  7.305254e-03  1.086335e-01
#> hyperConfidence        0.0000000  9.601859e-01  1.000000e+00  1.000000e+00
#> hyperLift              1.0000000  9.948652e-01  1.168992e+00  2.255269e+00
#> imbalance              0.0871146  8.590593e-01  8.003221e-01  6.024363e-01
#> implicationIndex       0.0000000 -1.601836e+00 -8.624380e+00 -2.275482e+01
#> importance             0.2613891  8.380387e-03  1.020920e-01  5.144888e-01
#> improvement            0.9128854  1.637120e-02  8.636884e-01  9.441176e-01
#> jaccard                0.9128854  1.223169e-01  1.517713e-01  3.554817e-01
#> jMeasure               0.0000000  2.172097e-04  8.880664e-03  1.055050e-01
#> kappa                  0.0000000  4.886481e-03  6.161820e-02  3.948413e-01
#> kulczynski             0.9564427  5.263616e-01  5.095922e-01  6.536199e-01
#> lambda                 0.0000000  0.000000e+00  0.000000e+00  3.416290e-01
#> laplace                0.9127653  9.282297e-01  8.628571e-01  9.432485e-01
#> leastContradiction     0.9045722  1.140672e-01  1.309548e-01  3.416290e-01
#> lerman                 0.0000000  4.948292e-01  5.576076e+00  2.871764e+01
#> leverage               0.0000000  1.985686e-03  2.012140e-02  8.283840e-02
#> LIC                          Inf  1.017933e+00           Inf           Inf
#> maxconfidence          1.0000000  9.292566e-01  8.636884e-01  9.441176e-01
#> mutualInformation            NaN  8.289227e-04  2.622346e-02  2.934492e-01
#> netconf                      NaN  1.863097e-02  1.815295e-01  6.556955e-01
#> oddsRatio                    NaN  1.289208e+00  2.952221e+00  4.168169e+01
#> phi                          NaN  2.156848e-02  1.325518e-01  4.788000e-01
#> ralambondrainy         0.0871146  8.580570e-03  1.730657e-02  8.289703e-03
#> relativeRisk                 NaN  1.020460e+00  1.266110e+00  3.273388e+00
#> rhsSupport             0.9128854  9.128854e-01  7.052065e-01  3.856894e-01
#> RLD                           NA  1.879271e-01  5.376032e-01  9.090324e-01
#> rulePowerFactor        0.8333598  1.047373e-01  9.470929e-02  1.322259e-01
#> precision              0.9128854  9.292566e-01  8.636884e-01  9.441176e-01
#> recall                 1.0000000  1.234666e-01  1.554960e-01  3.631222e-01
#> fScore                 0.9544591  2.179722e-01  2.635442e-01  5.245098e-01
#> sebag                 10.4791319  1.313559e+01  6.336134e+00  1.689474e+01
#> stdLift                1.0000000  5.969945e-01  3.184422e-01  7.205882e-01
#> table.n11           6277.0000000  7.750000e+02  7.540000e+02  9.630000e+02
#> table.n01              0.0000000  5.502000e+03  4.095000e+03  1.689000e+03
#> table.n10            599.0000000  5.900000e+01  1.190000e+02  5.700000e+01
#> table.n00              0.0000000  5.400000e+02  1.908000e+03  4.167000e+03
#> varyingLiaison         0.0000000  1.793346e-02  2.247312e-01  1.447871e+00
#> zhang                  0.0000000  1.664773e-02  1.686951e-01  6.945062e-01
#> yuleQ                        NaN  1.263353e-01  4.939554e-01  9.531415e-01
#> yuleY                        NaN  6.342171e-02  2.642197e-01  7.317645e-01
#>                              [,5]
#> support              1.364165e-01
#> confidence           9.196078e-01
#> lift                 1.007364e+00
#> count                9.380000e+02
#> addedValue           6.722445e-03
#> boost                1.007364e+00
#> causalConfidence     8.913565e-01
#> causalSupport        2.116056e-01
#> accuracy             2.116056e-01
#> balancedAccuracy     5.062698e-01
#> centeredConfidence   6.722445e-03
#> certainty            7.716783e-02
#> chiSquared           6.805848e-01
#> collectiveStrength   1.012069e+00
#> confirmedConfidence  8.392157e-01
#> conviction           1.083621e+00
#> cosine               3.707035e-01
#> counterexample       9.125800e-01
#> coverage             1.483421e-01
#> doc                  7.893362e-03
#> fishersExactTest     2.236385e-01
#> gini                 1.574286e-05
#> hyperConfidence      7.763615e-01
#> hyperLift            9.873684e-01
#> imbalance            8.267023e-01
#> implicationIndex    -7.274143e-01
#> importance           3.422807e-03
#> improvement          6.722445e-03
#> jaccard              1.475075e-01
#> jMeasure             4.316924e-05
#> kappa                2.523369e-03
#> kulczynski           5.345211e-01
#> lambda               0.000000e+00
#> laplace              9.187867e-01
#> leastContradiction   1.363709e-01
#> lerman               2.247083e-01
#> leverage             9.972213e-04
#> LIC                  1.007364e+00
#> maxconfidence        9.196078e-01
#> mutualInformation    1.706534e-04
#> netconf              7.893362e-03
#> oddsRatio            1.107694e+00
#> phi                  9.948857e-03
#> ralambondrainy       1.192554e-02
#> relativeRisk         1.008658e+00
#> rhsSupport           9.128854e-01
#> RLD                  7.716783e-02
#> rulePowerFactor      1.254497e-01
#> precision            9.196078e-01
#> recall               1.494344e-01
#> fScore               2.570920e-01
#> sebag                1.143902e+01
#> stdLift              5.980392e-01
#> table.n11            9.380000e+02
#> table.n01            5.339000e+03
#> table.n10            8.200000e+01
#> table.n00            5.170000e+02
#> varyingLiaison       7.363952e-03
#> zhang                8.360571e-03
#> yuleQ                5.109543e-02
#> yuleY                2.556441e-02

## calculate measures on a different set of transactions (I use a sample here)
## Note: reuse = TRUE (default) would just return the stored support on the
##   data set used for mining
newTrans <- sample(Income, 100)
m2 <- interestMeasure(rules, "support", transactions = newTrans, reuse = FALSE)
head(m2)
#> [1] 0.86 0.15 0.12 0.18 0.15 0.13

## calculate all available measures for the 5 frequent itemsets with highest support
its <- apriori(Income, parameter = list(target = "frequent itemsets"))
#> Apriori
#> 
#> Parameter specification:
#>  confidence minval smax arem  aval originalSupport maxtime support minlen
#>          NA    0.1    1 none FALSE            TRUE       5     0.1      1
#>  maxlen            target  ext
#>      10 frequent itemsets TRUE
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
#> checking subsets of size 1 2 3 4 5 6 7 8 done [0.05s].
#> sorting transactions ... done [0.00s].
#> writing ... [5571 set(s)] done [0.00s].
#> creating S4 object  ... done [0.00s].
its <- head(its, 5, by = "support")
inspect(its)
#>     items                             support count
#> [1] {language in home=english}      0.9128854  6277
#> [2] {education=no college graduate} 0.7052065  4849
#> [3] {number in household=1}         0.6918266  4757
#> [4] {ethnic classification=white}   0.6697208  4605
#> [5] {ethnic classification=white,                  
#>      language in home=english}      0.6595404  4535

interestMeasure(its, transactions = Income)
#>     support count allConfidence crossSupportRatio     lift
#> 1 0.9128854  6277     0.9128854         1.0000000 1.000000
#> 2 0.7052065  4849     0.7052065         1.0000000 1.000000
#> 3 0.6918266  4757     0.6918266         1.0000000 1.000000
#> 4 0.6697208  4605     0.6697208         1.0000000 1.000000
#> 5 0.6595404  4535     0.7224789         0.7336307 1.078776
```
