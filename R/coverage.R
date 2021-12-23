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


#' Calculate coverage for rules
#'
#' Provides the generic function and the needed S4 method to calculate the
#' coverage (support of the left-hand-side) of [rules].
#'
#' Coverage (also called cover or LHS-support) is the support of the
#' left-hand-side of the rule, i.e., \eqn{supp(X)}. It represents a measure of
#' to how often the rule can be applied.
#'
#' Coverage is quickly calculated from the rules quality measures (support and
#' confidence) stored in the quality slot. If these values are not present,
#' then the support of the LHS is counted using the data supplied in
#' [transactions].
#'
#' Coverage is also one of the measures available via the function
#' [interestMeasure()].
#'
#' @aliases coverage coverage,rules-method
#' @family interest measures
#' 
#' @param x the set of [rules].
#' @param transactions the data set used to generate `x`. Only needed if the
#' quality slot of 'x' does not contain support and confidence.
#' @param reuse reuse support and confidence stored in `x` or recompute from
#' transactions?
#' @return A numeric vector of the same length as `x` containing the
#' coverage values for the sets in `x`.
#' @author Michael Hahsler
#' @keywords models
#' @examples
#' data("Income")
#'
#' ## find and some rules (we only use 5 rules here) and calculate coverage
#' rules <- apriori(Income)[1:5]
#' quality(rules) <- cbind(quality(rules), coverage = coverage(rules))
#'
#' inspect(rules)
#' @export coverage
setGeneric("coverage",
  function(x,
    transactions = NULL,
    reuse = TRUE)
    standardGeneric("coverage"))

#' @rdname coverage
setMethod("coverage", signature(x = "rules"),
  function(x,
    transactions = NULL,
    reuse = TRUE) {
    q <- quality(x)
    
    if (reuse && all(c("support", "confidence") %in% names(q)))
      return(q$support / q$confidence)
    
    if (is.null(transactions))
      stop("transactions needed!")
    
    ## we need to calculate lhs-support
    return(support(lhs(x), transactions))
  })