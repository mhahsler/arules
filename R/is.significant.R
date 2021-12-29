#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#' Find Significant Rules
#'
#' Provides the generic functions and the S4 method `is.significant()` to
#' find significant associations and an implementation for rules.
#'
#' The implementation for association rules uses Fisher's exact test with
#' correction for multiple comparisons to test the null hypothesis that the LHS
#' and the RHS of the rule are independent.  Significant rules have a p-value
#' less then the specified significance level alpha (the null hypothesis of
#' independence is rejected.).
#'
#' @family interest measures
#' @family postprocessing
#' @family associations functions
#' 
#' @param x a set of rules.
#' @param transactions set of transactions used to mine the rules.
#' @param method test to use. Options are `"fisher"`, `"chisq"`. Note that
#' the contingency table is likely to have cells with low expected values and
#' that thus Fisher's Exact Test might be more appropriate than the chi-squared
#' test.
#' @param alpha required significance level.
#' @param adjust method to adjust for multiple comparisons. Options are
#' `"none"`, `"bonferroni"`, `"holm"`, `"fdr"`, etc. (see
#' [stats::p.adjust()])
#' @return returns a logical vector indicating which rules are significant.
#' @author Michael Hahsler
#' @seealso [stats::p.adjust()]
#' @references Hahsler, Michael and Kurt Hornik (2007). New probabilistic
#' interest measures for association rules. _Intelligent Data Analysis_,
#' 11(5):437--455.
#' @keywords manip
#' @examples
#' data("Income")
#' rules <- apriori(Income, parameter = list(support = 0.5))
#' is.significant(rules, Income)
#'
#' inspect(rules[is.significant(rules, Income)])
setGeneric("is.significant",
  function(x,
    transactions,
    method = "fisher",
    alpha = 0.01,
    adjust = "bonferroni")
    standardGeneric("is.significant"))

#' @rdname is.significant
setMethod("is.significant", signature(x = "rules"),
  function(x,
    transactions,
    method = "fisher",
    alpha = 0.01,
    adjust = "bonferroni") {
    methods <- c("fisher", "chisq")
    m <- pmatch(tolower(method), methods)
    if (is.na(m))
      stop("Unknown method.")
    method <- methods[m]
    
    if (method == "fisher")
      p <- interestMeasure(x,
        measure = "fishersExactTest",
        transactions = transactions,
        reuse = TRUE)
    
    ### chisq
    else
      p <- interestMeasure(
        x,
        measure = "chiSquared",
        transactions = transactions,
        reuse = TRUE,
        significance = TRUE
      )
    
    if (adjust != "none")
      p <- stats::p.adjust(p, method = adjust)
    p <= alpha
  })
