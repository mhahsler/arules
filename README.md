
# <img src="man/figures/logo.svg" align="right" height="139" /> R package arules - Mining Association Rules and Frequent Itemsets

[![Package on
CRAN](https://www.r-pkg.org/badges/version/arules)](https://CRAN.R-project.org/package=arules)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/arules)](https://CRAN.R-project.org/package=arules)
![License](https://img.shields.io/cran/l/arules)
[![Anaconda.org](https://anaconda.org/conda-forge/r-arules/badges/version.svg)](https://anaconda.org/conda-forge/r-arules)
[![r-universe
status](https://mhahsler.r-universe.dev/badges/arules)](https://mhahsler.r-universe.dev/arules)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-arules-orange.svg)](https://stackoverflow.com/questions/tagged/arules)

## Introduction

The arules package family for R provides the infrastructure for
representing, manipulating and analyzing transaction data and patterns
using [frequent itemsets and association
rules](https://en.wikipedia.org/wiki/Association_rule_learning). The
package also provides a wide range of [interest
measures](https://mhahsler.github.io/arules/docs/measures) and mining
algorithms including the code of Christian Borgelt’s popular and
efficient C implementations of the association mining algorithms
[Apriori](https://borgelt.net/apriori.html) and
[Eclat](https://borgelt.net/eclat.html). In addition, the following
mining algorithms are available via
[fim4r](https://borgelt.net/fim4r.html):

- Apriori
- Eclat
- Carpenter
- FPgrowth
- IsTa
- RElim
- SaM

Code examples can be found in [Chapter 5 of the web book R Companion for
Introduction to Data
Mining](https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/association-analysis-basic-concepts-and-algorithms.html).

To cite package ‘arules’ in publications use:

> Hahsler M, Gruen B, Hornik K (2005). “arules - A Computational
> Environment for Mining Association Rules and Frequent Item Sets.”
> *Journal of Statistical Software*, *14*(15), 1-25. ISSN 1548-7660,
> <doi:10.18637/jss.v014.i15> <https://doi.org/10.18637/jss.v014.i15>.

    @Article{,
      title = {arules -- {A} Computational Environment for Mining Association Rules and Frequent Item Sets},
      author = {Michael Hahsler and Bettina Gruen and Kurt Hornik},
      year = {2005},
      journal = {Journal of Statistical Software},
      volume = {14},
      number = {15},
      pages = {1--25},
      doi = {10.18637/jss.v014.i15},
      month = {October},
      issn = {1548-7660},
    }

## Packages

### arules core packages

- [arules](https://cran.r-project.org/package=arules): arules base
  package with data structures, mining algorithms (APRIORI and ECLAT),
  interest measures.
- [arulesViz](https://github.com/mhahsler/arulesViz): Visualization of
  association rules.
- [arulesCBA](https://github.com/mhahsler/arulesCBA): Classification
  algorithms based on association rules (includes CBA).  
- [arulesSequences](https://cran.r-project.org/package=arulesSequences):
  Mining frequent sequences (cSPADE).

### Other related packages

Additional mining algorithms

- [arulesNBMiner](https://github.com/mhahsler/arulesNBMiner): Mining
  NB-frequent itemsets and NB-precise rules.
- [fim4r](https://borgelt.net/fim4r.html): Provides fast implementations
  for several mining algorithms. An interface function called `fim4r()`
  is provided in `arules`.
- [opusminer](https://cran.r-project.org/package=opusminer): OPUS Miner
  algorithm for finding the op k productive, non-redundant itemsets.
  Call `opus()` with `format = 'itemsets'`.
- [RKEEL](https://cran.r-project.org/package=RKEEL): Interface to KEEL’s
  association rule mining algorithm.
- [RSarules](https://cran.r-project.org/package=RSarules): Mining
  algorithm which randomly samples association rules with one pre-chosen
  item as the consequent from a transaction dataset.

In-database analytics

- [ibmdbR](https://cran.r-project.org/package=ibmdbR): IBM in-database
  analytics for R can calculate association rules from a database table.
- [rfml](https://cran.r-project.org/package=rfml): Mine frequent
  itemsets or association rules using a MarkLogic server.

Interface

- [rattle](https://cran.r-project.org/package=rattle): Provides a
  graphical user interface for association rule mining.
- [pmml](https://cran.r-project.org/package=pmml): Generates PMML
  (predictive model markup language) for association rules.

Classification

- [arc](https://cran.r-project.org/package=arc): Alternative CBA
  implementation.
- [inTrees](https://cran.r-project.org/package=inTrees): Interpret Tree
  Ensembles provides functions for: extracting, measuring and pruning
  rules; selecting a compact rule set; summarizing rules into a learner.
- [rCBA](https://cran.r-project.org/package=rCBA): Alternative CBA
  implementation.
- [qCBA](https://cran.r-project.org/package=qCBA): Quantitative
  Classification by Association Rules.
- [sblr](https://cran.r-project.org/package=sbrl): Scalable Bayesian
  rule lists algorithm for classification.

Outlier Detection

- [fpmoutliers](https://cran.r-project.org/package=fpmoutliers):
  Frequent Pattern Mining Outliers.

Recommendation/Prediction

- [recommenerlab](https://github.com/mhahsler/recommenderlab): Supports
  creating predictions using association rules.

The following R packages use `arules`:
[arc](https://CRAN.R-project.org/package=arc),
[arlclustering](https://CRAN.R-project.org/package=arlclustering),
[arulesCBA](https://CRAN.R-project.org/package=arulesCBA),
[arulesNBMiner](https://CRAN.R-project.org/package=arulesNBMiner),
[arulesSequences](https://CRAN.R-project.org/package=arulesSequences),
[arulesViz](https://CRAN.R-project.org/package=arulesViz),
[clickstream](https://CRAN.R-project.org/package=clickstream),
[CLONETv2](https://CRAN.R-project.org/package=CLONETv2),
[CRE](https://CRAN.R-project.org/package=CRE),
[ctsem](https://CRAN.R-project.org/package=ctsem),
[discnorm](https://CRAN.R-project.org/package=discnorm),
[fcaR](https://CRAN.R-project.org/package=fcaR),
[fdm2id](https://CRAN.R-project.org/package=fdm2id),
[GroupBN](https://CRAN.R-project.org/package=GroupBN),
[ibmdbR](https://CRAN.R-project.org/package=ibmdbR),
[inTrees](https://CRAN.R-project.org/package=inTrees),
[nuggets](https://CRAN.R-project.org/package=nuggets),
[opusminer](https://CRAN.R-project.org/package=opusminer),
[pervasive](https://CRAN.R-project.org/package=pervasive),
[pmml](https://CRAN.R-project.org/package=pmml),
[qCBA](https://CRAN.R-project.org/package=qCBA),
[RareComb](https://CRAN.R-project.org/package=RareComb),
[rattle](https://CRAN.R-project.org/package=rattle),
[rCBA](https://CRAN.R-project.org/package=rCBA),
[recommenderlab](https://CRAN.R-project.org/package=recommenderlab),
[rgnoisefilt](https://CRAN.R-project.org/package=rgnoisefilt),
[RKEEL](https://CRAN.R-project.org/package=RKEEL),
[RulesTools](https://CRAN.R-project.org/package=RulesTools),
[sbrl](https://CRAN.R-project.org/package=sbrl),
[SurvivalTests](https://CRAN.R-project.org/package=SurvivalTests),
[TELP](https://CRAN.R-project.org/package=TELP)

## Installation

**Stable CRAN version:** Install from within R with

``` r
install.packages("arules")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/arules)

``` r
install.packages("arules",
    repos = c("https://mhahsler.r-universe.dev",
              "https://cloud.r-project.org/"))
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
rules <- apriori(trans, supp = 0.1, conf = 0.9, target = "rules")
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
    ## set transactions ...[84 item(s), 8993 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [42 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.02s].
    ## writing ... [457 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

Inspect the rules with the highest lift.

``` r
inspect(head(rules, n = 3, by = "lift"))
```

    ## Warning in seq.default(length = NCOL(quality)): partial argument match of
    ## 'length' to 'length.out'

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

## Using arules with tidyverse

`arules` works seamlessly with [tidyverse](https://www.tidyverse.org/).
For example:

- `dplyr` can be used for cleaning and preparing the transactions.
- `transaction()` and other functions accept `tibble` as input.
- Functions in arules can be connected with the pipe operator `|>`.
- [arulesViz](https://github.com/mhahsler/arulesViz) provides
  visualizations based on `ggplot2`.

For example, we can remove the ethnic information column before creating
transactions and then mine and inspect rules.

``` r
library("tidyverse")
library("arules")
data("IncomeESL")

trans <- IncomeESL |>
    select(-`ethnic classification`) |>
    transactions()
rules <- trans |>
    apriori(supp = 0.1, conf = 0.9, target = "rules", control = list(verbose = FALSE))
rules |>
    head(3, by = "lift") |>
    as("data.frame") |>
    tibble()
```

    ## # A tibble: 3 × 6
    ##   rules                                  support confidence coverage  lift count
    ##   <chr>                                    <dbl>      <dbl>    <dbl> <dbl> <int>
    ## 1 {dual incomes=no,householder status=o…   0.102      0.971    0.105  2.62   914
    ## 2 {years in bay area=>10,dual incomes=y…   0.100      0.961    0.104  2.59   902
    ## 3 {dual incomes=yes,householder status=…   0.110      0.960    0.114  2.59   988

## Using arules from Python

`arules` and `arulesViz` can now be used directly from Python with the
Python package [`arulespy`](https://pypi.org/project/arulespy/)
available form PyPI.

## Support

Please report bugs [here on
GitHub.](https://github.com/mhahsler/arules/issues) Questions should be
posted on [stackoverflow and tagged with
arules](https://stackoverflow.com/questions/tagged/arules).

## References

- Michael Hahsler. [ARULESPY: Exploring association rules and frequent
  itemsets in Python.](http://dx.doi.org/10.48550/arXiv.2305.15263)
  arXiv:2305.15263 \[cs.DB\], May 2023.
- Michael Hahsler. [An R Companion for Introduction to Data Mining:
  Chapter
  5](https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/association-analysis-basic-concepts.html),
  2021, URL:
  <https://mhahsler.github.io/Introduction_to_Data_Mining_R_Examples/book/>
- Hahsler, Michael. [A Probabilistic Comparison of Commonly Used
  Interest Measures for Association
  Rules](https://mhahsler.github.io/arules/docs/measures), 2015, URL:
  <https://mhahsler.github.io/arules/docs/measures>.
- Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian
  Buchta. [The arules R-package ecosystem: Analyzing interesting
  patterns from large transaction
  datasets.](https://jmlr.csail.mit.edu/papers/v12/hahsler11a.html)
  *Journal of Machine Learning Research,* 12:1977-1981, 2011.
- Michael Hahsler, Bettina Grün and Kurt Hornik. [arules - A
  Computational Environment for Mining Association Rules and Frequent
  Item Sets.](https://dx.doi.org/10.18637/jss.v014.i15) *Journal of
  Statistical Software,* 14(15), 2005.
