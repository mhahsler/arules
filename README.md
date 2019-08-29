# arules --- Mining Association Rules and Frequent Itemsets with R

[![CRAN version](http://www.r-pkg.org/badges/version/arules)](https://cran.r-project.org/package=arules)
 [![Rdoc](http://www.rdocumentation.org/badges/version/arules)](http://www.rdocumentation.org/packages/arules) 
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arules)](https://cran.r-project.org/package=arules)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/arules.svg?branch=master)](https://travis-ci.org/mhahsler/arules)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/arules?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/arules)

The arules package for R provides the infrastructure for representing,
manipulating and analyzing transaction data and patterns
using [frequent itemsets and association rules](https://en.wikipedia.org/wiki/Association_rule_learning). Also provides a wide range of 
[interest measures](https://michael.hahsler.net/research/association_rules/measures.html) and mining algorithms including an interfaces to
Borgelt's C implementations of the association mining algorithms [Apriori](http://www.borgelt.net/apriori.html) and [Eclat](http://www.borgelt.net/eclat.html).

### arules core packages: 

* [arules](https://cran.r-project.org/package=arules): arules base package with data structures, mining algorithms (APRIORI and ECLAT), interest measures. 
* [arulesViz](https://cran.r-project.org/package=arulesViz): Visualization of association rules. 
* [arulesCBA](https://cran.r-project.org/package=arulesCBA): Classification algorithms based on association rules (includes CBA).  
* [arulesSequences](https://cran.r-project.org/package=arulesSequences):
   Mining frequent sequences (cSPADE).

### Other related packages:

#### Additional mining algorithms 

* [arulesNBMiner](https://cran.r-project.org/package=arulesNBMiner):
  Mining NB-frequent itemsets and NB-precise rules.
* [opusminer](https://cran.r-project.org/package=opusminer): OPUS Miner algorithm for filtered top-k association discovery.
* [RKEEL](https://cran.r-project.org/package=RKEEL): Interface to KEEL's association rule mining algorithm.
* [RSarules](https://cran.r-project.org/package=RSarules): Mining algorithm which randomly samples association rules with one pre-chosen item as the consequent from a transaction dataset.


#### In-database analytics

* [ibmdbR](https://cran.r-project.org/package=ibmdbR): IBM in-database analytics for R can calculate association rules from a database table.
* [rfml](https://cran.r-project.org/package=rfml): Mine frequent itemsets or association rules using a MarkLogic server. 

#### Interface

* [rattle](https://cran.r-project.org/package=rattle): Provides a graphical user interface for association rule mining.
* [pmml](https://cran.r-project.org/package=pmml): Generates PMML (predictive model markup language) for association rules.

#### Classification 

* [arc](https://cran.r-project.org/package=arc): Alternative CBA implementation. 
* [inTrees](https://cran.r-project.org/package=inTrees): Interpret Tree Ensembles provides functions for: extracting, measuring and pruning rules; selecting a compact rule set; summarizing rules into a learner.
* [rCBA](https://cran.r-project.org/package=rCBA): Alternative CBA implementation.
* [qCBA](https://cran.r-project.org/package=qCBA): Quantitative Classification by Association Rules.
* [sblr](https://cran.r-project.org/package=sbrl): Scalable Bayesian rule lists algorithm for classification.

#### Outlier Detection

* [fpmoutliers](https://cran.r-project.org/package=fpmoutliers): Frequent Pattern Mining Outliers.

#### Recommendation/Prediction

* [recommenerlab](https://cran.r-project.org/package=recommenderlab): Supports creating predictions using association rules.


## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("arules")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/arules/build/artifacts) or install from GitHub (needs devtools). 
```R 
library("devtools")
install_github("mhahsler/arules")
``` 

## Usage

Load package and mine some association rules.
```R
library("arules")
data("Adult")

rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
```

```
Parameter specification:
 confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
        0.9    0.1    1 none FALSE            TRUE     0.5      1     10  rules FALSE

Algorithmic control:
 filter tree heap memopt load sort verbose
    0.1 TRUE TRUE  FALSE TRUE    2    TRUE

Absolute minimum support count: 24421 

apriori - find association rules with the apriori algorithm
version 4.21 (2004.05.09)        (c) 1996-2004   Christian Borgelt
set item appearances ...[0 item(s)] done [0.00s].
set transactions ...[115 item(s), 48842 transaction(s)] done [0.03s].
sorting and recoding items ... [9 item(s)] done [0.00s].
creating transaction tree ... done [0.03s].
checking subsets of size 1 2 3 4 done [0.00s].
writing ... [52 rule(s)] done [0.00s].
creating S4 object  ... done [0.01s].
```

Show basic statistics.
```R
summary(rules)
```

```
set of 52 rules

rule length distribution (lhs + rhs):sizes
 1  2  3  4 
 2 13 24 13 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.923   3.250   4.000 

summary of quality measures:
    support         confidence          lift            count      
 Min.   :0.5084   Min.   :0.9031   Min.   :0.9844   Min.   :24832  
 1st Qu.:0.5415   1st Qu.:0.9155   1st Qu.:0.9937   1st Qu.:26447  
 Median :0.5974   Median :0.9229   Median :0.9997   Median :29178  
 Mean   :0.6436   Mean   :0.9308   Mean   :1.0036   Mean   :31433  
 3rd Qu.:0.7426   3rd Qu.:0.9494   3rd Qu.:1.0057   3rd Qu.:36269  
 Max.   :0.9533   Max.   :0.9583   Max.   :1.0586   Max.   :46560  

mining info:
  data ntransactions support confidence
 Adult         48842     0.5        0.9
```

Inspect rules with the highest lift.
```R
inspect(head(rules, by = "lift"))
```

```
    lhs                               rhs                              support confidence     lift
[1] {sex=Male,                                                                                    
     native-country=United-States} => {race=White}                   0.5415421  0.9051090 1.058554
[2] {sex=Male,                                                                                    
     capital-loss=None,                                                                           
     native-country=United-States} => {race=White}                   0.5113632  0.9032585 1.056390
[3] {race=White}                   => {native-country=United-States} 0.7881127  0.9217231 1.027076
[4] {race=White,                                                                                  
     capital-loss=None}            => {native-country=United-States} 0.7490480  0.9205626 1.025783
[5] {race=White,                                                                                  
     sex=Male}                     => {native-country=United-States} 0.5415421  0.9204803 1.025691
[6] {race=White,                                                                                  
     capital-gain=None}            => {native-country=United-States} 0.7194628  0.9202807 1.025469
```


## Support

Please report bugs [here on GitHub.](https://github.com/mhahsler/arules/issues)
Questions should be posted on [stackoverflow and tagged with arules](https://stackoverflow.com/questions/tagged/arules).


## References

* [Intro article with examples](https://cran.r-project.org/package=arules/vignettes/arules.pdf) by Michael Hahsler, Bettina Gr&uuml;n, Kurt Hornik and
Christian Buchta.
* Michael Hahsler, Bettina Gr&uuml;n and Kurt Hornik, [arules - A Computational Environment for Mining Association Rules and Frequent Item Sets.](http://dx.doi.org/10.18637/jss.v014.i15) _Journal of Statistical Software,_ 14(15), 2005.
* Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian Buchta. [The arules R-package ecosystem: Analyzing interesting patterns from large transaction datasets.](http://jmlr.csail.mit.edu/papers/v12/hahsler11a.html) _Journal of Machine Learning Research,_ 12:1977-1981, 2011.
* Hahsler, Michael (2015). 
[A Probabilistic Comparison of Commonly Used Interest Measures for Association Rules](http://michael.hahsler.net/research/association_rules/measures.html), 2015, URL: http://michael.hahsler.net/research/association_rules/measures.html.
