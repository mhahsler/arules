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

#' List Representation for Objects Based on Class itemMatrix
#'
#' Provides the generic function `LIST()` and the methods to create a
#' list representation from objects of the classes [itemMatrix],
#' [transactions], and [tidLists].
#'
#' Using `LIST()` with `decode = TRUE` is equivalent to the standard
#' coercion `as(x, "list")`.  `LIST` returns the object `from`
#' as a list of vectors.  Each vector represents one row of the
#' [itemMatrix] (e.g., items in a transaction).
#'
#' @family import/export
#'
#' @param from the object to be converted into a list.
#' @param ... further arguments.
#' @param decode a logical controlling whether the items/transactions are
#' decoded from the column numbers internally used by
#' [itemMatrix] to the names stored in the object
#' `from`.  The default behavior is to decode.
#' @return a list primitive.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data(Adult)
#'
#' ### default coercion (same as as(Adult[1:5], "list"))
#' LIST(Adult[1:5])
#'
#' ### coercion without item decoding
#' LIST(Adult[1:5], decode = FALSE)
setGeneric(
  "LIST",
  function(from, ...) {
    standardGeneric("LIST")
  }
)

#' @rdname LIST
setMethod(
  "LIST", signature(from = "itemMatrix"),
  function(from, decode = TRUE) {
    l <-
      .Call(R_asList_ngCMatrix, from@data, if (decode) {
        itemLabels(from)
      } else {
        NULL
      })
    if (decode) {
      names(l) <- itemsetInfo(from)[["itemsetID"]]
    }
    l
  }
)

#' @rdname LIST
setMethod(
  "LIST", signature(from = "transactions"),
  function(from, decode = TRUE) {
    l <- LIST(as(from, "itemMatrix"), decode)
    if (decode) {
      names(l) <- transactionInfo(from)$transactionID
    }
    l
  }
)


#' @rdname LIST
setMethod(
  "LIST", signature(from = "tidLists"),
  function(from, decode = TRUE) {
    if (decode) {
      i <- from@transactionInfo[["transactionID"]]
      if (!is.null(i)) {
        i <- as.character(i)
      }
      to <- .Call(R_asList_ngCMatrix, from@data, i)
      names(to) <- from@itemInfo[["labels"]]
      to
    } else {
      .Call(R_asList_ngCMatrix, from@data, NULL)
    }
  }
)
