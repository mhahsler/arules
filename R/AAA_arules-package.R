#' Infrastructure for representing transaction data, mining frequent itemsets
#' and association rules, and evaluating the resulting patterns. The package
#' includes efficient C implementations of the Apriori and Eclat algorithms.
#'
#' @section Typical workflow:
#'
#' 1. Create a [transactions] object with [transactions()] or import basket or
#'    single-format data with [read.transactions()]. Numeric variables should
#'    first be converted to categories with [discretize()] or [discretizeDF()].
#' 2. Inspect the data with [summary()], [inspect()], [itemFrequency()], or
#'    [itemFrequencyPlot()].
#' 3. Mine association rules with [apriori()] or frequent itemsets with
#'    [eclat()].
#' 4. Rank and filter patterns with [sort()], [subset()], and
#'    [interestMeasure()]. Use [is.redundant()], [is.closed()], or
#'    [is.maximal()] for common postprocessing tasks.
#' 5. Convert results with [DATAFRAME()] or [LIST()], or visualize them with the
#'    suggested package arulesViz.
#'
#' @section Choosing a mining function:
#'
#' - [apriori()] mines rules or itemsets and offers detailed control over rule
#'   appearance.
#' - [eclat()] is designed for mining frequent, closed, or maximal itemsets.
#' - [ruleInduction()] creates rules from an existing collection of itemsets.
#' - [fim4r()] provides access to additional mining algorithms when the optional
#'   fim4r package is installed.
#'
#' @section Core classes:
#'
#' [transactions] stores sparse binary transaction data. Mining functions return
#' [itemsets] or [rules], both derived from [associations]. Item coding must be
#' compatible when objects are compared or combined; see [itemCoding].
#'
#' @seealso [arulesViz](https://github.com/mhahsler/arulesViz) for visualization
#' and the package vignettes for extended examples.
#' @keywords internal
#'
#' @useDynLib arules, .registration=TRUE
"_PACKAGE"
