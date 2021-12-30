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

#' Adding Items to Data
#'
#' Provides the generic function `merge()` and the methods for [itemMatrix]
#' and [transactions] to add new items to existing data.
#'
#' @family preprocessing
#' @family itemMatrix and transactions functions
#' 
#' @param x an object of class [itemMatrix] or [transactions].
#' @param y an object of the same class as `x` (or something which can be coerced
#' to that class).
#' @param ...  further arguments; unused.
#' @return Returns a new object of the same class as `x` with the items in `y`
#' added.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Groceries")
#'
#' ## create a random item as a matrix
#' randomItem <- sample(c(TRUE, FALSE), size = length(Groceries),replace = TRUE)
#' randomItem <- as.matrix(randomItem)
#' colnames(randomItem) <- "random item"
#' head(randomItem, 3)
#'
#' ## add the random item to Groceries
#' g2 <- merge(Groceries, randomItem)
#' nitems(Groceries)
#' nitems(g2)
#' inspect(head(g2, 3))
#'
setGeneric("merge")

#' @rdname merge
setMethod("merge", signature(x = "itemMatrix"),
  function(x, y, ...) {
    y <- as(y, "itemMatrix")
    if (nrow(x) != nrow(y))
      stop("The number of rows in x and y do not conform!")
    
    ## this is faster than dc <- rbind(x@data, y@data)
    dc <- t(.Call(R_cbind_ngCMatrix, t(x@data), t(y@data)))
    
    ## fix itemInfo
    iix <- itemInfo(x)
    iiy <- itemInfo(y)
    names <- unique(union(colnames(iix), colnames(iiy)))
    for (n in names) {
      if (is.null(iix[[n]]))
        iix[[n]] <- NA_character_
      if (is.null(iiy[[n]]))
        iiy[[n]] <- NA_character_
    }
    
    ii <- rbind(iix, iiy)
    
    new(
      "itemMatrix",
      data        = dc,
      itemInfo    = ii,
      itemsetInfo = itemsetInfo(x)
    )
  })

#' @rdname merge
setMethod("merge", signature(x = "transactions"),
  function(x, y, ...) {
    m <- merge(as(x, "itemMatrix"), as(y, "itemMatrix"))
    as(m, "transactions")
  })
