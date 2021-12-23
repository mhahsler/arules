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


# setGeneric("image") # is generic in Matrix

## NOTE Matrix coerces to dgTMatrix and then uses levelplot
## FIXME: Matrix does not correctly pass on labels from ngCMatrix so
## 		we manually coerce to dgTMatrix

#' Visual Inspection of Binary Incidence Matrices
#' 
#' Provides the S4 methods \code{image} to generate level plots to visually
#' inspect binary incidence matrices, i.e., objects based on
#' [itemMatrix] (e.g., transactions, tidLists, items in
#' itemsets or rhs/lhs in rules).  These plots can be used to identify problems
#' in a data set (e.g., recording problems with some transactions containing
#' all items).
#' 
#' @name image
#' @family itemMatrix and transactions functions
#' 
#' @aliases image
#' @param x the object ([itemMatrix], [transactions] or
#' [tidLists]).
#' @param xlab,ylab labels for the plot.
#' @param ... further arguments passed on to \code{image} in package
#' \pkg{Matrix} which in turn are passed on to \code{levelplot} in
#' \pkg{lattice}.
#' @author Michael Hahsler
#' @seealso image in package \pkg{Matrix}
#' @keywords hplot
#' @examples
#' data("Epub")
#' 
#' ## in this data set we can see that not all
#' ## items were available from the beginning.
#' image(Epub[1:1000])
NULL

#' @rdname image
setMethod("image", signature(x = "itemMatrix"),
    function(x, xlab = "Items (Columns)", ylab = "Elements (Rows)", ...) 
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
	sub = NULL, ylab = ylab, xlab = xlab, ...)
)

#' @rdname image
setMethod("image", signature(x = "transactions"),
    function(x, xlab = "Items (Columns)", ylab = "Transactions (Rows)" ,...)
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
	sub = NULL, ylab = ylab, xlab = xlab, ...)
)

#' @rdname image
setMethod("image", signature(x = "tidLists"),
    function(x, xlab="Transactions (Columns)",
        ylab="Items/itemsets (Rows)", ...)
    Matrix::image(as(t(as(x, "ngCMatrix")), "dgCMatrix"), 
            ylab = ylab, xlab = xlab, ...)
)
