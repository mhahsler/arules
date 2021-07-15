# arules — Mining Association Rules and Frequent Itemsets with R

[![CRAN
version](https://www.r-pkg.org/badges/version/arules)](https://cran.r-project.org/package=arules)
[![Rdoc](https://www.rdocumentation.org/badges/version/arules)](https://www.rdocumentation.org/packages/arules)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/arules)](https://cran.r-project.org/package=arules)
[![R build
status](https://github.com/mhahsler/arules/workflows/R-CMD-check/badge.svg)](https://github.com/mhahsler/arules/actions)

The arules package for R provides the infrastructure for representing,
manipulating and analyzing transaction data and patterns using [frequent
itemsets and association
rules](https://en.wikipedia.org/wiki/Association_rule_learning). Also
provides a wide range of [interest
measures](https://mhahsler.github.io/arules/docs/measures) and mining
algorithms including a interfaces and the code of Christian Borgelt’s
popular and efficient C implementations of the association mining
algorithms [Apriori](https://borgelt.net/apriori.html) and
[Eclat](https://borgelt.net/eclat.html).

## arules core packages:

-   [arules](https://cran.r-project.org/package=arules): arules base
    package with data structures, mining algorithms (APRIORI and ECLAT),
    interest measures.
-   [arulesViz](https://github.com/mhahsler/arulesViz): Visualization of
    association rules.
-   [arulesCBA](https://github.com/ianjjohnson/arulesCBA):
    Classification algorithms based on association rules (includes
    CBA).  
-   [arulesSequences](https://cran.r-project.org/package=arulesSequences):
    Mining frequent sequences (cSPADE).

## Other related packages:

### Additional mining algorithms

-   [arulesNBMiner](https://github.com/mhahsler/arulesNBMiner): Mining
    NB-frequent itemsets and NB-precise rules.
-   [opusminer](https://cran.r-project.org/package=opusminer): OPUS
    Miner algorithm for filtered top-k association discovery.
-   [RKEEL](https://cran.r-project.org/package=RKEEL): Interface to
    KEEL’s association rule mining algorithm.
-   [RSarules](https://cran.r-project.org/package=RSarules): Mining
    algorithm which randomly samples association rules with one
    pre-chosen item as the consequent from a transaction dataset.

### In-database analytics

-   [ibmdbR](https://cran.r-project.org/package=ibmdbR): IBM in-database
    analytics for R can calculate association rules from a database
    table.
-   [rfml](https://cran.r-project.org/package=rfml): Mine frequent
    itemsets or association rules using a MarkLogic server.

### Interface

-   [rattle](https://cran.r-project.org/package=rattle): Provides a
    graphical user interface for association rule mining.
-   [pmml](https://cran.r-project.org/package=pmml): Generates PMML
    (predictive model markup language) for association rules.

### Classification

-   [arc](https://cran.r-project.org/package=arc): Alternative CBA
    implementation.
-   [inTrees](https://cran.r-project.org/package=inTrees): Interpret
    Tree Ensembles provides functions for: extracting, measuring and
    pruning rules; selecting a compact rule set; summarizing rules into
    a learner.
-   [rCBA](https://cran.r-project.org/package=rCBA): Alternative CBA
    implementation.
-   [qCBA](https://cran.r-project.org/package=qCBA): Quantitative
    Classification by Association Rules.
-   [sblr](https://cran.r-project.org/package=sbrl): Scalable Bayesian
    rule lists algorithm for classification.

### Outlier Detection

-   [fpmoutliers](https://cran.r-project.org/package=fpmoutliers):
    Frequent Pattern Mining Outliers.

### Recommendation/Prediction

-   [recommenerlab](https://github.com/mhahsler/recommenderlab):
    Supports creating predictions using association rules.

## Installation

**Stable CRAN version:** install from within R with

``` r
install.packages("arules")
```

**Current development version:** install from GitHub (needs devtools and
\[Rtools for Windows\]
(<https://cran.r-project.org/bin/windows/Rtools/>)).

``` r
devtools::install_github("mhahsler/arules")
```

## Usage

Load package and mine some association rules.

``` r
library("arules")
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
data("IncomeESL")

trans <- transactions(IncomeESL)
trans
```

    ## transactions in sparse format with
    ##  8993 transactions (rows) and
    ##  84 items (columns)

``` r
rules <- apriori(trans, parameter = list(supp = 0.1, conf = 0.9, target = "rules"))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.9    0.1    1 none FALSE            TRUE       5     0.1      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 899 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[84 item(s), 8993 transaction(s)] done [0.01s].
    ## sorting and recoding items ... [42 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.03s].
    ## writing ... [457 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

Show basic statistics.

``` r
summary(rules)
```

    ## set of 457 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##   2   3   4   5   6 
    ##  17 130 218  88   4 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     2.0     3.0     4.0     3.9     4.0     6.0 
    ## 
    ## summary of quality measures:
    ##     support       confidence      coverage         lift          count     
    ##  Min.   :0.10   Min.   :0.90   Min.   :0.10   Min.   :1.04   Min.   : 900  
    ##  1st Qu.:0.11   1st Qu.:0.92   1st Qu.:0.12   1st Qu.:1.08   1st Qu.: 982  
    ##  Median :0.12   Median :0.95   Median :0.13   Median :1.11   Median :1103  
    ##  Mean   :0.14   Mean   :0.95   Mean   :0.15   Mean   :1.41   Mean   :1273  
    ##  3rd Qu.:0.15   3rd Qu.:0.97   3rd Qu.:0.16   3rd Qu.:1.65   3rd Qu.:1377  
    ##  Max.   :0.61   Max.   :1.00   Max.   :0.65   Max.   :2.62   Max.   :5495  
    ## 
    ## mining info:
    ##   data ntransactions support confidence
    ##  trans          8993     0.1        0.9
    ##                                                                               call
    ##  apriori(data = trans, parameter = list(supp = 0.1, conf = 0.9, target = "rules"))

Inspect rules with the highest lift.

``` r
inspect(head(rules, n = 3, by = "lift"))
```

    ##     lhs                           rhs                      support confidence coverage lift count
    ## [1] {dual incomes=no,                                                                            
    ##      householder status=own}   => {marital status=married}    0.10       0.97     0.10  2.6   914
    ## [2] {years in bay area=>10,                                                                      
    ##      dual incomes=yes,                                                                           
    ##      type of home=house}       => {marital status=married}    0.10       0.96     0.10  2.6   902
    ## [3] {dual incomes=yes,                                                                           
    ##      householder status=own,                                                                     
    ##      type of home=house,                                                                         
    ##      language in home=english} => {marital status=married}    0.11       0.96     0.11  2.6   988

## Using arule and tidyverse

arules works seamlessly with [tidyverse](https://www.tidyverse.org/).
For example, dplyr can be used for cleaning and preparing the
transactions and then functions in arules can be used with `%>%`.

``` r
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.2     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x tidyr::expand() masks Matrix::expand()
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()
    ## x tidyr::pack()   masks Matrix::pack()
    ## x dplyr::recode() masks arules::recode()
    ## x tidyr::unpack() masks Matrix::unpack()

``` r
library("arules")
data("IncomeESL")

trans <- IncomeESL %>% transactions()

rules <- trans %>% apriori(parameter = list(supp = 0.1, conf = 0.9, target = "rules"))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.9    0.1    1 none FALSE            TRUE       5     0.1      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 899 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[84 item(s), 8993 transaction(s)] done [0.01s].
    ## sorting and recoding items ... [42 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.02s].
    ## writing ... [457 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
rules %>% head(n = 3, by = "lift") %>% inspect()
```

    ##     lhs                           rhs                      support confidence coverage lift count
    ## [1] {dual incomes=no,                                                                            
    ##      householder status=own}   => {marital status=married}    0.10       0.97     0.10  2.6   914
    ## [2] {years in bay area=>10,                                                                      
    ##      dual incomes=yes,                                                                           
    ##      type of home=house}       => {marital status=married}    0.10       0.96     0.10  2.6   902
    ## [3] {dual incomes=yes,                                                                           
    ##      householder status=own,                                                                     
    ##      type of home=house,                                                                         
    ##      language in home=english} => {marital status=married}    0.11       0.96     0.11  2.6   988

## Usage arules from Python

See [Getting started with R arules using
Python.](https://mhahsler.github.io/arules/docs/python/arules_python.html)

## Support

Please report bugs [here on
GitHub.](https://github.com/mhahsler/arules/issues) Questions should be
posted on [stackoverflow and tagged with
arules](https://stackoverflow.com/questions/tagged/arules).

## References

-   Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian
    Buchta. [The arules R-package ecosystem: Analyzing interesting
    patterns from large transaction
    datasets.](https://jmlr.csail.mit.edu/papers/v12/hahsler11a.html)
    *Journal of Machine Learning Research,* 12:1977-1981, 2011.
-   Michael Hahsler, Bettina Grün and Kurt Hornik. [arules - A
    Computational Environment for Mining Association Rules and Frequent
    Item Sets.](https://dx.doi.org/10.18637/jss.v014.i15) *Journal of
    Statistical Software,* 14(15), 2005.
-   Hahsler, Michael. [A Probabilistic Comparison of Commonly Used
    Interest Measures for Association
    Rules](https://michael.hahsler.net/research/association_rules/measures.html),
    2015, URL:
    <https://michael.hahsler.net/research/association_rules/measures.html>.
