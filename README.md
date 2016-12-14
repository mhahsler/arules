# arules --- Mining Association Rules and Frequent Itemsets with R

[![CRAN version](http://www.r-pkg.org/badges/version/arules)](https://cran.r-project.org/package=arules)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/arules)](https://cran.r-project.org/package=arules)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/arules.svg?branch=master)](https://travis-ci.org/mhahsler/arules)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/arules?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/arules)

The arules package for R provides the infrastructure for representing,
manipulating and analyzing transaction data and patterns (frequent
itemsets and association rules). Also provides interfaces to
C implementations of the association mining algorithms Apriori and Eclat.

Additional packages in the arules family are: 

* [arulesViz](http://github.com/mhahsler/arulesViz): Visualization of association rules. 
* [arulesCBA](http://github.com/ianjjohnson/arulesCBA): Classification based on association rules.  
* [arulesSequences](https://cran.r-project.org/package=arulesSequences):
   Mining frequent sequences.
* [arulesNBMiner](http://github.com/mhahsler/arulesNBMiner):
  Mining NB-frequent itemsets and NB-precise rules.

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("arules")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/arules/build/artifacts) or install from GitHub (needs devtools). 
```R 
install_git("mhahsler/arules")
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

rule length distribution (lhs + rhs):
 1  2  3  4 
 2 13 24 13 

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.000   3.000   2.923   3.250   4.000 

summary of quality measures:
    support         confidence          lift       
 Min.   :0.5084   Min.   :0.9031   Min.   :0.9844  
 1st Qu.:0.5415   1st Qu.:0.9155   1st Qu.:0.9937  
 Median :0.5974   Median :0.9229   Median :0.9997  
 Mean   :0.6436   Mean   :0.9308   Mean   :1.0036  
 3rd Qu.:0.7426   3rd Qu.:0.9494   3rd Qu.:1.0057  
 Max.   :0.9533   Max.   :0.9583   Max.   :1.0586  

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

## References

* [Intro article with examples](https://cran.r-project.org/package=arules/vignettes/arules.pdf) by Michael Hahsler, Bettina Gr&uuml;n, Kurt Hornik and
Christian Buchta.
* Michael Hahsler, Bettina Gr&uuml;n and Kurt Hornik, [arules - A Computational Environment for Mining Association Rules and Frequent Item Sets.](http://dx.doi.org/10.18637/jss.v014.i15) _Journal of Statistical Software,_ 14(15), 2005.
* Michael Hahsler, Sudheer Chelluboina, Kurt Hornik, and Christian Buchta. [The arules R-package ecosystem: Analyzing interesting patterns from large transaction datasets.](http://jmlr.csail.mit.edu/papers/v12/hahsler11a.html) _Journal of Machine Learning Research,_ 12:1977-1981, 2011.
