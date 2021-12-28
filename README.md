# arules — Mining Association Rules and Frequent Itemsets with R

[![CRAN
version](https://www.r-pkg.org/badges/version/arules)](https://cran.r-project.org/package=arules)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/arules)](https://cran.r-project.org/package=arules)
[![R build
status](https://github.com/mhahsler/arules/workflows/R-CMD-check/badge.svg)](https://github.com/mhahsler/arules/actions)

The arules package for R provides the infrastructure for representing,
manipulating and analyzing transaction data and patterns using [frequent
itemsets and association
rules](https://en.wikipedia.org/wiki/Association_rule_learning). The
package also provides a wide range of [interest
measures](https://mhahsler.github.io/arules/docs/measures) and mining
algorithms including the code of Christian Borgelt’s
popular and efficient C implementations of the association mining
algorithms [Apriori](https://borgelt.net/apriori.html) and
[Eclat](https://borgelt.net/eclat.html). In addition the following algorithms are
available via [fim4r](https://borgelt.net/fim4r.html):

* Apriori
* Eclat
* Carpenter
* FPgrowth
* IsTa 
* RElim 
* SaM

Code examples can be found in
[Chapter 5 of the web book R Companion for Introduction to Data
Mining](https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/association-analysis-basic-concepts-and-algorithms.html).

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
-   [fim4r](https://borgelt.net/fim4r.html): Provides fast implementations for several
      mining algorithms. An interface function called `fim4r()` is provided in `arules`.
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
[Rtools for Windows](https://cran.r-project.org/bin/windows/Rtools/)).

``` r
devtools::install_github("mhahsler/arules")
```

## Usage

Load package and mine some association rules.

``` r
library("arules")
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

Inspect the rules with the highest lift.

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
    ## checking subsets of size 1 2 3 4 5 6 done [0.03s].
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

## Using arules from Python

See [Getting started with arules using
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
