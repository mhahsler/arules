# Infrastructure for representing transaction data, mining frequent itemsets and association rules, and evaluating the resulting patterns. The package includes efficient C implementations of the Apriori and Eclat algorithms.

Provides the infrastructure for representing, manipulating and analyzing
transaction data and patterns (frequent itemsets and association rules).
Also provides C implementations of the association mining algorithms
Apriori and Eclat. Hahsler, Gruen and Hornik (2005)
[doi:10.18637/jss.v014.i15](https://doi.org/10.18637/jss.v014.i15) .

## Typical workflow

1.  Create a
    [transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
    object with
    [`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
    or import basket or single-format data with
    [`read.transactions()`](http://michael.hahsler.net/arules/reference/read.md).
    Numeric variables should first be converted to categories with
    [`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md)
    or
    [`discretizeDF()`](http://michael.hahsler.net/arules/reference/discretize.md).

2.  Inspect the data with
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md),
    [`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md),
    or
    [`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md).

3.  Mine association rules with
    [`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
    or frequent itemsets with
    [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md).

4.  Rank and filter patterns with
    [`sort()`](http://michael.hahsler.net/arules/reference/sort.md),
    [`subset()`](http://michael.hahsler.net/arules/reference/subset.md),
    and
    [`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md).
    Use
    [`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md),
    [`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md),
    or
    [`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md)
    for common postprocessing tasks.

5.  Convert results with
    [`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md)
    or [`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md),
    or visualize them with the suggested package arulesViz.

## Choosing a mining function

- [`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
  mines rules or itemsets and offers detailed control over rule
  appearance.

- [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md) is
  designed for mining frequent, closed, or maximal itemsets.

- [`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)
  creates rules from an existing collection of itemsets.

- [`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md)
  provides access to additional mining algorithms when the optional
  fim4r package is installed.

## Core classes

[transactions](http://michael.hahsler.net/arules/reference/transactions-class.md)
stores sparse binary transaction data. Mining functions return
[itemsets](http://michael.hahsler.net/arules/reference/itemsets-class.md)
or [rules](http://michael.hahsler.net/arules/reference/rules-class.md),
both derived from
[associations](http://michael.hahsler.net/arules/reference/associations-class.md).
Item coding must be compatible when objects are compared or combined;
see
[itemCoding](http://michael.hahsler.net/arules/reference/itemCoding.md).

## See also

[arulesViz](https://github.com/mhahsler/arulesViz) for visualization and
the package vignettes for extended examples.

## Author

**Maintainer**: Michael Hahsler <mhahsler@lyle.smu.edu>
([ORCID](https://orcid.org/0000-0003-2716-1405)) \[copyright holder\]

Authors:

- Michael Hahsler <mhahsler@lyle.smu.edu>
  ([ORCID](https://orcid.org/0000-0003-2716-1405)) \[copyright holder\]

- Christian Buchta \[copyright holder\]

- Bettina Gruen \[copyright holder\]

- Kurt Hornik ([ORCID](https://orcid.org/0000-0003-4198-9911))
  \[copyright holder\]

Other contributors:

- Christian Borgelt \[contributor, copyright holder\]

- Ian Johnson \[contributor\]

- Makhlouf Ledmi \[contributor\]
