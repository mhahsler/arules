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

#' Itemwise Set Operations
#'
#' Provides the generic functions and the methods for itemwise set
#' operations on items in an [itemMatrix]. The regular set operations regard each
#' itemset in an `itemMatrix` as an element. Itemwise operations regard each item
#' as an element and operate on the items of pairs of corresponding itemsets
#' (first itemset in `x` with first itemset in `y`, second with second, etc.).
#'
#' @name itemwiseSetOps
#' @aliases itemSetOperations 
#' @family itemMatrix and transactions Functions
#'
#' @param x,y two [itemMatrix] objects with the same number of rows (itemsets).
#' @return An object of class [itemMatrix] is returned.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#'
#' fsets <- eclat(Adult, parameter = list(supp = 0.5))
#' inspect(fsets[1:4])
#' inspect(itemUnion(items(fsets[1:2]), items(fsets[3:4])))
#' inspect(itemSetdiff(items(fsets[1:2]), items(fsets[3:4])))
#' inspect(itemIntersect(items(fsets[1:2]), items(fsets[3:4])))
NULL

#' @rdname itemwiseSetOps
setGeneric("itemUnion",
  function(x, y)
    standardGeneric("itemUnion"))

#' @rdname itemwiseSetOps
setGeneric("itemSetdiff",
  function(x, y)
    standardGeneric("itemSetdiff"))

#' @rdname itemwiseSetOps
setGeneric("itemIntersect",
  function(x, y)
    standardGeneric("itemIntersect"))


#' @rdname itemwiseSetOps
setMethod("itemUnion", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if (length(x) != length(y))
      stop("Length mismatch between x and y!")
    
    ### the C code does not deal well with a large number of dense rules.
    #x@data <- .Call(R_or_ngCMatrix", x@data, y@data)
    
    x@data <- as(x@data | y@data, "nsparseMatrix")
    
    x
  })

#' @rdname itemwiseSetOps
setMethod("itemSetdiff", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if (length(x) != length(y))
      stop("Length mismatch between x and y!")
    
    x@data <-
      as(drop0(x@data - y@data > 0), "nsparseMatrix")
    x
  })

#' @rdname itemwiseSetOps
setMethod("itemIntersect", signature(x = "itemMatrix", y = "itemMatrix"),
  function(x, y) {
    if (length(x) != length(y))
      stop("Length mismatch between x and y!")
    
    x@data <- as(x@data & y@data, "nsparseMatrix")
    x
  })
