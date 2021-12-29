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

#' Find Super and Subsets
#'
#' Provides the generic functions and the S4 methods `is.subset()` and
#' `is.superset()` for finding super or subsets in [associations] and
#' [itemMatrix] objects.
#'
#' looks for each element in `x` which elements in `y` are supersets
#' or subsets. Note that the method can be very slow and memory intensive if
#' `x` and/or `y` contain many elements.
#'
#' For rules, the union of lhs and rhs is used a the set of items.
#'
#' @aliases is.superset
#' @family postprocessing
#' @family associations functions
#' @family itemMatrix and transactions functions
#'
#' @param x,y associations or itemMatrix objects. If `y = NULL`, the super
#' or subset structure within set `x` is calculated.
#' @param proper a logical indicating if all or just proper super or subsets.
#' @param sparse a logical indicating if a sparse (ngCMatrix) rather than a
#' dense logical matrix should be returned. Sparse computation preserves a
#' significant amount of memory and is much faster for large sets.
#' @param \dots currently unused.
#' @return returns a logical matrix or a sparse ngCMatrix (for
#' `sparse = TRUE`) with `length(x)` rows and `length(y)` columns.
#' Each logical row vector represents which elements in `y` are supersets
#' (subsets) of the corresponding element in `x`.  If either `x` or
#' `y` have length zero, `NULL` is returned instead of a matrix.
#' @author Michael Hahsler and Ian Johnson
#' @keywords manip
#' @examples
#' data("Adult")
#' set <- eclat(Adult, parameter = list(supp = 0.8))
#'
#' ### find the supersets of each itemset in set
#' is.superset(set, set)
#' is.superset(set, set, sparse = FALSE)
setGeneric("is.superset",
  function(x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE,
    ...)
    standardGeneric("is.superset"))

#' @rdname is.superset
setGeneric("is.subset",
  function(x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE,
    ...)
    standardGeneric("is.subset"))

#' @rdname is.superset
setMethod("is.superset", signature(x = "itemMatrix"),
  function(x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE)
    if (is.null(y))
      t(is.subset(x, NULL, proper, sparse))
  else
    t(is.subset(y, x, proper, sparse)))

#' @rdname is.superset
setMethod("is.superset", signature(x = "associations"),
  function (x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE)
    if (is.null(y))
      t(is.subset(x, NULL, proper, sparse))
  else
    t(is.subset(y, x, proper, sparse)))

## this takes about 3 times the memory but is very fast!
## I suspect internally it always uses a lot of memory.

#' @rdname is.superset
setMethod("is.subset", signature(x = "itemMatrix"),
  function(x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE) {
    if (length(x) == 0 || (!is.null(y) && length(y) == 0))
      return(logical(0))
    
    ## y needs to be itemMatrix and x has to conform!
    if (!is.null(y)) {
      if (is(y, "associations"))
        y <- items(y)
      if (!is(y, "itemMatrix"))
        stop("y needs to be an itemMatrix.")
      il <- union(itemLabels(x), itemLabels(y))
      x <- recode(x, itemLabels = il)
      y <- recode(y, itemLabels = il)
    }
    
    if (sparse)
      return(.is.subset_sparse(x, y, proper))
    
    if (is.null(y))
      m <- .Call(R_crosstab_ngCMatrix, x@data, NULL, FALSE)
    else
      m <- .Call(R_crosstab_ngCMatrix, x@data, y@data, FALSE)
    
    m <- m == size(x)
    
    if (proper == TRUE)
      if (is.null(y))
        m <- m & outer(size(x), size(x), "<")
    else
      m <- m & outer(size(x), size(y), "<")
    
    rownames(m) <- labels(x)
    if (is.null(y))
      colnames(m) <- labels(x)
    else
      colnames(m) <- labels(y)
    
    m
  })

#' @rdname is.superset
setMethod("is.subset", signature(x = "associations"),
  function(x,
    y = NULL,
    proper = FALSE,
    sparse = TRUE)
    is.subset(items(x), y, proper, sparse))

### use tidlist intersection
.is.subset_sparse <- function(x, y = NULL, proper = FALSE) {
  if (is.null(y))
    y <- x
  
  p <- as.integer(rep(0, x@data@Dim[2] + 1))
  i <- .Call(
    R_is_subset,
    x@data@p,
    x@data@i,
    x@data@Dim,
    y@data@p,
    y@data@i,
    y@data@Dim,
    as.logical(proper),
    p,
    PACKAGE = "arules"
  )
  
  t(new(
    "ngCMatrix",
    p = p,
    i = i,
    Dim = c(y@data@Dim[2], x@data@Dim[2]),
    Dimnames = list(labels(y), labels(x))
  ))
}


### convert a list into a ngCMatrix
.list2ngCMatrix <- function(from, max = NULL) {
  from <- lapply(from, sort)
  p <- cumsum(sapply(from, length))
  
  i <- as.integer(unlist(from, use.names = FALSE))
  
  if (is.null(max))
    max <- max(i)
  
  t(new(
    "ngCMatrix",
    p   = c(0L, p),
    i   = i - 1L,
    Dim = c(as.integer(max), length(from))
  ))
}
