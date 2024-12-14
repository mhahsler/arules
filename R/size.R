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


#' Number of Items in Sets
#'
#' Provides the generic function `size()` and methods to get the size of
#' each itemset in an [itemMatrix] or [associations]. For
#' example, `size()` can be used to get a vector with the number of items in each
#' transaction.
#'
#' @include itemMatrix.R
#' @family itemMatrix and transactions functions
#' @family associations functions
#'
#' @param x an object.
#' @param ... further (unused) arguments.
#' @return returns a numeric vector of length `length(x)`.
#' Each element is the size of the corresponding element (row in the [itemMatrix]) in
#' object `x`. For [rules], `size()` returns the sum of the number of
#' items in the LHS and the RHS.
#' @author Michael Hahsler
#' @keywords attribute
#' @examples
#' data("Adult")
#' summary(size(Adult))
#'
setGeneric(
  "size",
  function(x, ...) standardGeneric("size")
)

## FIXME: Add transactionID or itemsetID as names?

#' @rdname size
setMethod(
  "size", signature(x = "itemMatrix"),
  function(x) colSums(x@data)
)

#' @rdname size
setMethod(
  "size", signature(x = "tidLists"),
  function(x) colSums(x@data)
)

#' @rdname size
setMethod(
  "size", signature(x = "itemsets"),
  function(x) colSums(x@items@data)
)

#' @rdname size
setMethod(
  "size", signature(x = "rules"),
  function(x) size(x@lhs) + size(x@rhs)
)
