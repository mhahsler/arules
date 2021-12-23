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



setGeneric("sort")

#' Sort Associations
#'
#' Provides the method `sort` to sort elements in class
#' [associations] (e.g., itemsets or rules) according to the
#' value of measures stored in the association's slot `quality` (e.g.,
#' support).
#'
#' `sort` is relatively slow for large sets of associations since it has
#' to copy and rearrange a large data structure.  Note that sorting creates a
#' second copy of the set of associations which can be slow and memory
#' consuming for large sets. With `order = TRUE` a integer vector with the
#' order is returned instead of the reordered associations.
#'
#' If only the top \code{n} associations are needed then \code{head} using
#' \code{by} performs this faster than calling \code{sort} and then \code{head}
#' since it does it without copying and rearranging all the data.  \code{tail}
#' works in the same way.
#'
#' @include associations.R
#' @name sort
#' @aliases sort SORT
#' @family associations functions
#'  
#' @param x an object to be sorted.
#' @param decreasing a logical. Should the sort be increasing or decreasing?
#' (default is decreasing)
#' @param na.last na.last is not supported for associations.  NAs are always
#' put last.
#' @param by a character string specifying the quality measure stored in
#' \code{x} to be used to sort \code{x}. If a vector of character strings is
#' specified then the additional strings are used to sort \code{x} in case of
#' ties.
#' @param order should a order vector be returned instead of the sorted
#' associations?
#' @param ... Further arguments are ignored.
#' 
#' @return An object of the same class as `x`.
#' @author Michael Hahsler
#' @keywords manip arith
#' @examples
#' data("Adult")
#'
#' ## Mine rules with APRIORI
#' rules <- apriori(Adult, parameter = list(supp = 0.6))
#'
#' rules_by_lift <- sort(rules, by = "lift")
#'
#' inspect(head(rules))
#' inspect(head(rules_by_lift))
#'
#' ## A faster/less memory consuming way to get the top 5 rules according to lift
#' ## (see Details section)
#' inspect(head(rules, n = 5, by = "lift"))
NULL

#' @rdname sort
setMethod("sort", signature(x = "associations"),
  function (x,
    decreasing = TRUE,
    na.last = NA,
    by = "support",
    order = FALSE,
    ...) {
    if (!is.na(na.last))
      stop("na.last not supported. NAs are always put last.")
    
    q <- quality(x)
    m <- pmatch(by, colnames(q))
    if (any(is.na(m)))
      stop("Unknown interest measure to sort by.")
    
    q <- q[, m, drop = FALSE]
    
    if (length(x) == 0)
      return(x)
    
    o <- do.call(base::order, c(q, list(
      na.last = TRUE,
      decreasing = decreasing
    )))
    
    if (order)
      o
    else
      x[o]
  })
