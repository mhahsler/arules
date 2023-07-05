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
#' Provides the generic functions `is.significant()` and the method to
#' find significant [rules].
#'
#' The implementation for association rules uses Fisher's exact test with
#' correction for multiple comparisons to test the null hypothesis that the LHS
#' and the RHS of the rule are independent.  Significant rules have a p-value
#' less then the specified significance level alpha (the null hypothesis of
#' independence is rejected). See Hahsler and Hornik (2007) for details.
#'
#' @family interest measures
#' @family postprocessing
#' @family associations functions
#' 
#' @param x a set of [rules].
#' @param transactions optional set of [transactions]. Only needed if not sufficient
#'    interest measures are available in `x`. If the test should be performed
#'    on a transaction set different then the one used for mining (use `reuse = FALSE`). 
#' @param method test to use. Options are `"fisher"`, `"chisq"`. Note that
#' the contingency table is likely to have cells with low expected values and
#' that thus Fisher's Exact Test might be more appropriate than the chi-squared
#' test.
#' @param alpha required significance level.
#' @param adjust method to adjust for multiple comparisons. Some options are
#' `"none"`, `"bonferroni"`, `"holm"`, `"fdr"`, etc. (see
#' [stats::p.adjust()] for more methods)
#' @param reuse logical indicating if information in the quality slot should be 
#'   reuse for calculating the measures. 
#' @param ... further arguments are passed on to [interestMeasure()].
#' @return returns a logical vector indicating which rules are significant.
#' @author Michael Hahsler
#' @seealso [stats::p.adjust()]
#' @references Hahsler, Michael and Kurt Hornik (2007). New probabilistic
#' interest measures for association rules. _Intelligent Data Analysis_,
#' 11(5):437--455.
#' \doi{10.3233/IDA-2007-11502}
#' @keywords manip
#' @examples
#' data("Income")
#' rules <- apriori(Income, support = 0.2)
#' is.significant(rules)
#'
#' rules[is.significant(rules)]
#'
#' # Adjust P-values for multiple comparisons
#' rules[is.significant(rules, adjust = "bonferroni")]
setGeneric("is.significant",
  function(x, ...)
    standardGeneric("is.significant"))

#' @rdname is.significant
setMethod("is.significant", signature(x = "rules"),
  function(x,
    transactions = NULL,
    method = "fisher",
    alpha = 0.01,
    adjust = "none",
    reuse = TRUE,
    ...) {
    methods <- c("fisher", "chisq")
    m <- pmatch(tolower(method), methods)
    if (is.na(m))
      stop("Unknown method. Available methods are: ", 
           paste(sQuote(methods), collapse = ", "))
    method <- methods[m]
    
    if (method == "fisher")
      p <- interestMeasure(x,
        measure = "fishersExactTest",
        transactions = transactions,
        reuse = reuse,
        ...)
    
    ### chisq
    else
      p <- interestMeasure(
        x,
        measure = "chiSquared",
        transactions = transactions,
        significance = TRUE,
        reuse = reuse,
        ...
      )
    
    p <- stats::p.adjust(p, method = adjust)
    p <= alpha
  })
