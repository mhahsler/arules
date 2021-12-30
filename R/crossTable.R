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


#' Cross-tabulate joint occurrences across pairs of items
#'
#' Provides the generic function `crossTable()` and a method to
#' cross-tabulate joint occurrences across all pairs of items.
#'
#' @aliases crossTable
#' @family itemMatrix and transactions functions
#' 
#' @param x object to be cross-tabulated ([transactions] or
#' [itemMatrix]).
#' @param measure measure to return. Default is co-occurrence counts.
#' @param sort sort the items by support.
#' @param ...  additional arguments.
#' 
#' @return A symmetric matrix of n x n, where n is the number of items times
#' in `x`. The matrix contains the co-occurrence counts between pairs of
#' items.
#' @author Michael Hahsler
#' @keywords models
#' @examples
#' data("Groceries")
#'
#' ct <- crossTable(Groceries, sort = TRUE)
#' ct[1:5, 1:5]
#'
#' sp <- crossTable(Groceries, measure = "support", sort = TRUE)
#' sp[1:5, 1:5]
#'
#' lift <- crossTable(Groceries, measure = "lift", sort = TRUE)
#' lift[1:5, 1:5]
#'
setGeneric("crossTable", function(x, ...)
  standardGeneric("crossTable"))

#' @rdname crossTable
setMethod("crossTable", signature(x = "itemMatrix"),
  function(x,
    measure = c("count", "support", "probability", "lift"),
    sort = FALSE) {
    measure <- match.arg(measure)
    
    m <- .Call(R_crosstab_ngCMatrix, x@data, NULL, TRUE)
    if (is.null(dimnames(m)))
      dimnames(m) <- list(itemLabels(x), itemLabels(x))
    
    if (sort) {
      o <- order(diag(m), decreasing = TRUE)
      m <- m[o, o]
    }
    
    if (measure == "count")
      return(m)
    
    p <- m / nrow(x)
    if (measure %in% c("support", "probability"))
      return(p)
    
    if (measure == "lift") {
      p_items <- diag(p)
      diag(p) <- NA
      e <- outer(p_items, p_items, "*")
      return(p / e)
    }
    
    stop("Unknown measure!")
  })
