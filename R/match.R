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

#' Value Matching
#'
#' Provides the generic function `match()` and the methods for
#' [associations], [transactions] and [itemMatrix] objects.  `match()` returns a vector
#' of the positions of (first) matches of its first argument in its second.
#'
#' `%in%` is a more intuitive interface as a binary operator, which
#' returns a logical vector indicating if there is a match or not for the items
#' in the itemsets (left operand) with the items in the table (right operand).
#'
#' \pkg{arules} defines additional binary operators for matching itemsets:
#' `%pin%` uses _partial matching_ on the table; `%ain%`
#' itemsets have to match/include _all_ items in the table; `%oin%`
#' itemsets can _only_ match/include the items in the table.  The binary
#' matching operators or often used in [subset()].
#'
#' @aliases match
#' @family associations functions
#' @family itemMatrix and transactions functions
#'
#' @param x an object of class [itemMatrix], [transactions] or
#' [associations].
#' @param table a set of associations or transactions to be matched against.
#' @param nomatch the value to be returned in the case when no match is found.
#' @param incomparables not implemented.
#' @return `match`: An integer vector of the same length as `x`
#' giving the position in `table` of the first match if there is a match,
#' otherwise `nomatch`.
#'
#' `%in%`, `%pin%`, `%ain%`, `%oin%`: A logical vector,
#' indicating if a match was located for each element of `x`.
#' @author Michael Hahsler
#' @keywords manip
#' @examples
#' data("Adult")
#'
#' ## get unique transactions, count frequency of unique transactions
#' ## and plot frequency of unique transactions
#' vals <- unique(Adult)
#' cnts <- tabulate(match(Adult, vals))
#' plot(sort(cnts, decreasing = TRUE))
#'
#' ## find all transactions which are equal to transaction 10 in Adult
#' which(Adult %in% Adult[10])
#'
#' ## for transactions we can also match directly with itemLabels.
#' ## Find in the first 10 transactions the ones which
#' ## contain age=Middle-aged (see help page for class itemMatrix)
#' Adult[1:10] %in% "age=Middle-aged"
#'
#' ## find all transactions which contain items that partially match "age=" (all here).
#' Adult[1:10] %pin% "age="
#'
#' ## find all transactions that only include the item "age=Middle-aged" (none here).
#' Adult[1:10] %oin% "age=Middle-aged"
#'
#' ## find al transaction which contain both items "age=Middle-aged" and "sex=Male"
#' Adult[1:10] %ain% c("age=Middle-aged", "sex=Male")
setGeneric("match")

#' @rdname match
setMethod(
  "match", signature(x = "itemMatrix", table = "itemMatrix"),
  function(
      x,
      table,
      nomatch = NA_integer_,
      incomparables = NULL) {
    if (!compatible(x, table)) {
      warning("Item coding not compatible, recoding item matrices first.")

      k <- match(itemLabels(x), itemLabels(table))
      n <- which(is.na(k))
      if (length(n)) {
        k[n] <- table@data@Dim[1] + seq(length(n))
        table@data@Dim[1] <- table@data@Dim[1] + length(n)
      }
      if (any(k != seq_len(length(k)))) {
        x@data <- .Call(R_recode_ngCMatrix, x@data, k)
      }
      if (x@data@Dim[1] < table@data@Dim[1]) {
        x@data@Dim[1] <- table@data@Dim[1]
      }
    }

    i <- .Call(R_pnindex, table@data, x@data, FALSE)
    match(i,
      seq_len(length(table)),
      nomatch = nomatch,
      incomparables = incomparables
    )
  }
)

#' @rdname match
setMethod(
  "match", signature(x = "rules", table = "rules"),
  function(
      x,
      table,
      nomatch = NA_integer_,
      incomparables = NULL) {
    match(
      .joinedList(x),
      .joinedList(table),
      nomatch = nomatch,
      incomparables = incomparables
    )
  }
)

#' @rdname match
setMethod(
  "match", signature(x = "itemsets", table = "itemsets"),
  function(
      x,
      table,
      nomatch = NA_integer_,
      incomparables = NULL) {
    match(
      x@items,
      table@items,
      nomatch = nomatch,
      incomparables = incomparables
    )
  }
)

## find elements which contain some items (as labels or
## in itemInfo) note this is not what we would expect for
## %in% in R! but match below works the R-way

setGeneric("%in%")

#' @rdname match
#' @aliases %in%
setMethod(
  "%in%", signature(x = "itemMatrix", table = "itemMatrix"),
  function(x, table) {
    !is.na(match(x, table))
  }
)

#' @rdname match
setMethod(
  "%in%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos))) {
      stop("table contains an unknown item label")
    }
    size(x[, pos]) > 0
  }
)

#' @rdname match
setMethod(
  "%in%", signature(x = "associations", table = "associations"),
  function(x, table) {
    match(x, table)
  }
)

# partial in

setGeneric(
  "%pin%",
  function(x, table) standardGeneric("%pin%")
)

#' @rdname match
#' @aliases %pin%
setMethod(
  "%pin%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    if (length(table) > 1) {
      warning("table contains more than one item label pattern and only the first element will be used")
      table <- table[1]
    }

    if (table[1] == "") {
      stop("table contains an illegal pattern (empty string)")
    }

    pos <- grep(table, itemLabels(x))

    if (is.na(pos[1])) {
      return(rep(FALSE, length(x)))
    }

    size(x[, pos]) > 0
  }
)


# all in

setGeneric(
  "%ain%",
  function(x, table) standardGeneric("%ain%")
)

#' @rdname match
#' @aliases %ain%
setMethod(
  "%ain%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos))) {
      stop("table contains an unknown item label")
    }
    size(x[, pos]) == length(pos)
  }
)

## only items can to be in

setGeneric(
  "%oin%",
  function(x, table) standardGeneric("%oin%")
)

#' @rdname match
#' @aliases %oin%
setMethod(
  "%oin%", signature(x = "itemMatrix", table = "character"),
  function(x, table) {
    pos <- match(table, itemLabels(x))
    if (any(is.na(pos))) {
      stop("table contains an unknown item label")
    }
    size(x[, -pos]) == 0
  }
)
