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

#' Methods for "[": Extraction or Subsetting in Package 'arules'
#'
#' Methods for `"["`, i.e., extraction or subsetting in package
#' \pkg{arules}. Subsetting can be done by integers containing column/row
#' numbers, vectors of logicals or strings containing parts of item labels.
#'
#'
#' @name extract
#' @aliases "[-methods"
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x an object of class [itemMatrix], [transactions] or [associations].
#' @param i select rows/sets.
#' @param j select columns/items.
#' @param ... further arguments are ignored.
#' @param drop ignored.
#' 
#' @section Methods: \describe{ \item{[}{\code{signature(x = "itemMatrix", i =
#' "ANY", j = "ANY", drop= "ANY")}; extracts parts of an `itemMatrix`.
#' The first argument selects rows (e.g., transactions or rules) and the second
#' argument selects columns (items).  Either argument can be omitted to select
#' all rows or columns.} \item{[}{\code{signature(x = "itemsets", i = "ANY", j
#' = "ANY", drop= "ANY")}; extracts a subset of itemsets and the associated
#' quality measures.  `j` has to be missing. } \item{[}{\code{signature(x
#' = "rules", i = "ANY", j = "ANY", drop= "ANY")}; extracts a subset of rules
#' and the associated quality measures.  `j` has to be missing. }
#' \item{[}{\code{signature(x = "transactions", i = "ANY", j = "ANY", drop=
#' "ANY")}; extracts a subset of transactions/items from a transactions object
#' (a binary incidence matrix).  `i` and `j` can be numeric where
#' `i` selects transactions and `j` selects items. }
#' \item{[}{\code{signature(x = "tidLists", i = "ANY", j = "ANY", drop=
#' "ANY")}; extracts parts (transaction ID vectors) from `tidLists`.
#' `i` selects the items or itemsets and `j` selects transactions in
#' the lists.} }
#' @author Michael Hahsler
#' @keywords array
#' @examples
#' data(Adult)
#' Adult
#'
#' ## select first 10 transactions
#' Adult[1:10]
#'
#' ## select first 10 items for first 100 transactions
#' Adult[1:100, 1:10]
#'
#' ## select the first 100 transactions for the items containing
#' ## "income" or "age=Young" in their labels
#' Adult[1:100, c("income=small", "income=large" ,"age=Young")]
NULL

#' @rdname extract
setMethod("[", signature(
  x = "itemMatrix",
  i = "ANY",
  j = "ANY",
  drop = "ANY"
),
  function(x, i, j, ..., drop) {
    ## i and j are reversed internally!
    if (!missing(i)) {
      ## recycling?
      if (is.logical(i) &&
          length(i) != ncol(x))
        i <- rep(i, length.out = nrow(x))
      
      ## deal with NAs we do not take NAs
      if (any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if (is.logical(i))
          i[is.na(i)] <- FALSE
        else
          i <- i[!is.na(i)]
      }
      
      i <- .translate_index(i, rownames(x), nrow(x))
      ## faster than: x@data <- x@data[,i, drop=FALSE]
      x@data <- .Call(R_colSubset_ngCMatrix, x@data, i)
      
      ### only subset if we have rows
      if (nrow(x@itemsetInfo))
        x@itemsetInfo <- x@itemsetInfo[i, , drop = FALSE]
    }
    
    if (!missing(j)) {
      ## recycling?
      if (is.logical(j) &&
          length(j) != ncol(x))
        j <- rep(j, length.out = ncol(x))
      
      ## deal with NAs we do not take NAs
      if (any(is.na(j))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if (is.logical(j))
          j[is.na(j)] <- FALSE
        else
          j <- j[!is.na(j)]
      }
      
      j <- .translate_index(j, colnames(x), ncol(x))
      ## faster than: x@data <- x@data[j,, drop=FALSE]
      x@data <- .Call(R_rowSubset_ngCMatrix, x@data, j)
      
      x@itemInfo <- x@itemInfo[j, , drop = FALSE]
    }
    
    ## makes sure that items are still unique
    validObject(x, complete = TRUE)
    x
  })

#' @rdname extract 
setMethod("[", signature(
  x = "transactions",
  i = "ANY",
  j = "ANY",
  drop = "ANY"
),
  function(x, i, j, ..., drop) {
    ## i and j are reversed
    if (!missing(i)) {
      if (any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if (is.logical(i))
          i[is.na(i)] <- FALSE
        else
          i <- i[!is.na(i)]
      }
      
      x <- new("transactions", as(x, "itemMatrix")[i, , ..., drop])
    }
    
    if (!missing(j))
      x <- new("transactions",
        as(x, "itemMatrix")[, j, ..., drop],
        itemsetInfo = x@itemsetInfo)
    
    x
  })

#' @rdname extract
setMethod("[", signature(
  x = "tidLists",
  i = "ANY",
  j = "ANY",
  drop = "ANY"
),
  function(x, i, j, ..., drop) {
    ## i and j are reversed internally!
    if (!missing(i)) {
      if (any(is.na(i))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if (is.logical(i))
          i[is.na(i)] <- FALSE
        else
          i <- i[!is.na(i)]
      }
      
      i <- .translate_index(i, rownames(x), nrow(x))
      x@data <- .Call(R_colSubset_ngCMatrix, x@data, i)
      
      x@itemInfo <- x@itemInfo[i, , drop = FALSE]
    }
    
    if (!missing(j)) {
      if (any(is.na(j))) {
        warning("Subsetting with NAs. NAs are omitted!")
        if (is.logical(j))
          j[is.na(j)] <- FALSE
        else
          j <- j[!is.na(j)]
      }
      
      j <- .translate_index(j, colnames(x), ncol(x))
      x@data <- .Call(R_rowSubset_ngCMatrix, x@data, j)
      
      x@transactionInfo <- x@transactionInfo[j, , drop = FALSE]
    }
    
    validObject(x, complete = TRUE)
    x
  })


#' @rdname extract
setMethod("[", signature(
  x = "rules",
  i = "ANY",
  j = "ANY",
  drop = "ANY"
),
  function(x, i, j, ..., drop) {
    if (!missing(j))
      stop("incorrect dimension (j not possible)")
    if (missing(i))
      return(x)
    
    if (any(is.na(i))) {
      warning("Subsetting with NAs. NAs are omitted!")
      if (is.logical(i))
        i[is.na(i)] <- FALSE
      else
        i <- i[!is.na(i)]
    }
    
    slots <- intersect(slotNames(x), c("lhs", "rhs"))
    for (s in slots)
      slot(x, s) <- slot(x, s)[i]
    
    x@quality <- x@quality[i, , drop = FALSE]
    
    validObject(x)
    x
  })

#' @rdname extract
setMethod("[", signature(
  x = "itemsets",
  i = "ANY",
  j = "ANY",
  drop = "ANY"
),
  function(x, i, j, ..., drop) {
    if (!missing(j))
      stop("incorrect number of dimensions (j not possible)")
    if (missing(i))
      return(x)
    
    if (any(is.na(i))) {
      warning("Subsetting with NAs. NAs are omitted!")
      if (is.logical(i))
        i[is.na(i)] <- FALSE
      else
        i <- i[!is.na(i)]
    }
    
    slots <- intersect(slotNames(x), c("items", "tidLists"))
    for (sl in slots)
      slot(x, sl) <- slot(x, sl)[i]
    
    x@quality <- x@quality[i, , drop = FALSE]
    
    validObject(x)
    x
  })
