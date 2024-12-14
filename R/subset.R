#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
# 			Bettina Gruen and Kurt Hornik
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

#' Subsetting Itemsets, Rules and Transactions
#'
#' Provides the generic function `subset()` and methods to subset
#' [associations] or [transactions] ([itemMatrix]) which meet certain conditions
#' (e.g., contains certain items or satisfies a minimum lift).
#'
#' `subset()` finds the rows/itemsets/rules of `x` that match the expression
#' given in `subset`. Parts of `x` like items, lhs, rhs and the columns in the quality data.frame (e.g., support and lift) can be directly referred to by their names
#' in `subset`.
#'
#' Important operators to select itemsets containing items specified by their
#' labels are
#'
#' * [%in%]: select itemsets matching _any_ given item
#' * [%ain%]: select only itemsets matching _all_ given item
#' * [%oin%]: select only itemsets matching _only_ the given item
#' * [%pin%]: `%in%` with _partial matching_
#'
#' @aliases subset
#' @param x object to be subsetted.
#' @param subset logical expression indicating elements to keep.
#' @param ... further arguments to be passed to or from other methods.
#' @return An object of the same class as `x` containing only the elements
#' which satisfy the conditions.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#' rules <- apriori(Adult)
#'
#' ## select all rules with item "marital-status=Never-married" in
#' ## the right-hand-side and lift > 2
#' rules.sub <- subset(rules, subset = rhs %in% "marital-status=Never-married" &
#'   lift > 2)
#'
#' ## use partial matching for all items corresponding to the variable
#' ## "marital-status"
#' rules.sub <- subset(rules, subset = rhs %pin% "marital-status=")
#'
#' ## select only rules with items "age=Young" and "workclass=Private" in
#' ## the left-hand-side
#' rules.sub <- subset(rules, subset = lhs %ain%
#'   c("age=Young", "workclass=Private"))
setGeneric("subset")

#' @rdname subset
setMethod(
  "subset", signature(x = "itemMatrix"),
  function(x, subset, ...) {
    if (missing(subset)) {
      return(x)
    }
    i <- eval(
      substitute(subset), list(items = x),
      parent.frame(2)
    )
    x[i, ]
  }
)


#' @rdname subset
setMethod(
  "subset", signature(x = "itemsets"),
  function(x, subset, ...) {
    if (missing(subset)) {
      return(x)
    }
    i <- eval(
      substitute(subset), c(
        quality(x),
        list(items = items(x))
      ),
      parent.frame(2)
    )
    x[i, ]
  }
)

#' @rdname subset
setMethod(
  "subset", signature(x = "rules"),
  function(x, subset, ...) {
    if (missing(subset)) {
      return(x)
    }

    i <- eval(
      substitute(subset), c(
        quality(x),
        list(lhs = lhs(x), rhs = rhs(x), items = items(x))
      ),
      parent.frame(2)
    )
    x[i, ]
  }
)
